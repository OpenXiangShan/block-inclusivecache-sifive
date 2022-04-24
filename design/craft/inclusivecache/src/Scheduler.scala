/*
 * Copyright 2019 SiFive, Inc.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You should have received a copy of LICENSE.Apache2 along with
 * this software. If not, you may obtain a copy at
 *
 *    https://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package sifive.blocks.inclusivecache

import Chisel._
import freechips.rocketchip.diplomacy.AddressSet
import freechips.rocketchip.tilelink._
import freechips.rocketchip.util._

class TrackWire[T <: Data](gen: T) extends Module {
  val io = IO(new Bundle {
    val in = Input(gen)
    val out = Output(gen)
  })

  io.out := io.in
}

object TrackWire {
  def apply[T <: Data](in: T): T = {
    val trackWire = Module(new TrackWire(in))
    trackWire.io.in := in
    trackWire.io.out
  }
}


class ScheduleCut(params: InclusiveCacheParameters) extends Module {
  class ScheduleInfo extends Bundle {
    val schedule = new ScheduleRequest(params)
    val mshrStatus = new MSHRStatus(params)
    val mshrReqValids = UInt(params.mshrs.W)
    val mshrSelectOH = UInt(params.mshrs.W)
    val prioRequests = UInt((params.mshrs * 3).W)
  }

  val io = IO(new Bundle {
    val need_to_schedule = Input(Bool())
    val select = Input(new ScheduleInfo)
    val issue = Output(new ScheduleInfo)
    val s_select = Output(Bool())
    val s_issue = Output(Bool())
  })

  /**
    * schedule_state:
    *   s_select: under this state, scheduler pick a shedulable mshr, compute its index
    *   s_schedule: under this state, scheduler make the target modules see the selected mshr's request and shift the mshr.
    */
  val s_select = RegInit(true.B)
  val s_issue = RegInit(false.B)

  def IssueReg[T <: Data](t: T): T = {
    val r = Reg(t)
    when (reset || s_issue) {
      r := 0.U.asTypeOf(t)
    }
    when (s_select && io.need_to_schedule) {
      r := t
    }
    r
  }

  io.issue := IssueReg(io.select)

  when (s_select && io.need_to_schedule) {
    assert(s_select)
    assert(!s_issue)
    s_select := false.B
    s_issue := true.B
    DebugPrint(params, "shift_to_issue\n")
  }

  when (s_issue) {
    assert(!s_select)
    assert(s_issue)
    s_select := true.B
    s_issue := false.B
    DebugPrint(params, "shift_to_select\n")
  }

  io.s_select := s_select
  io.s_issue := s_issue
}

class Scheduler(params: InclusiveCacheParameters) extends Module with HasTLDump
{
  val io = new Bundle {
    val in = TLBundle(params.inner.bundle).flip
    val out = TLBundle(params.outer.bundle)
    // Way permissions
    val ways = Vec(params.allClients, UInt(width = params.cache.ways)).flip
    val divs = Vec(params.allClients, UInt(width = InclusiveCacheParameters.lfsrBits + 1)).flip
    // Control port
    val req = Decoupled(new SinkXRequest(params)).flip
    val resp = Decoupled(new SourceXRequest(params))
    val prefetcher = new PrefetcherIO(params.inner.bundle.addressBits)
  }

  val sourceA = Module(new SourceA(params))
  val sourceB = Module(new SourceB(params))
  val sourceC = Module(new SourceC(params))
  val sourceD = Module(new SourceD(params))
  val sourceE = Module(new SourceE(params))
  val sourceX = Module(new SourceX(params))

  io.out.a <> sourceA.io.a
  io.out.c <> sourceC.io.c
  io.out.e <> sourceE.io.e
  io.in.b <> sourceB.io.b
  io.in.d <> sourceD.io.d
  io.resp <> sourceX.io.x

  when (io.in.a.fire()) {
    DebugPrint(params, "inner acquire ")
    io.in.a.bits.dump(params)
  }
  when (io.in.b.fire()) {
    DebugPrint(params, "inner probe ")
    io.in.b.bits.dump(params)
  }

  when (io.in.c.fire()) {
    DebugPrint(params, "inner release ")
    io.in.c.bits.dump(params)
  }

  when (io.in.d.fire()) {
    DebugPrint(params, "inner grant ")
    io.in.d.bits.dump(params)
  }

  when (io.in.e.fire()) {
    DebugPrint(params, "inner finish ")
    io.in.e.bits.dump(params)
  }

  when (io.out.a.fire()) {
    DebugPrint(params, "outer acquire ")
    io.out.a.bits.dump(params)
  }
  when (io.out.b.fire()) {
    DebugPrint(params, "outer probe ")
    io.out.b.bits.dump(params)
  }

  when (io.out.c.fire()) {
    DebugPrint(params, "outer release ")
    io.out.c.bits.dump(params)
  }

  when (io.out.d.fire()) {
    DebugPrint(params, "outer grant ")
    io.out.d.bits.dump(params)
  }

  when (io.out.e.fire()) {
    DebugPrint(params, "outer finish ")
    io.out.e.bits.dump(params)
  }


  val sinkA = Module(new SinkA(params))
  val sinkB = Module(new SinkB(params))
  val sinkC = Module(new SinkC(params))
  val sinkD = Module(new SinkD(params))
  val sinkE = Module(new SinkE(params))
  val sinkX = Module(new SinkX(params))

  sinkA.io.a <> io.in.a
  sinkB.io.b <> io.out.b
  sinkC.io.c <> io.in.c
  sinkE.io.e <> io.in.e
  sinkD.io.d <> io.out.d
  sinkX.io.x <> io.req

  // io.out.b.ready := Bool(true) // disconnected

  val directory = Module(new Directory(params))
  val bankedStore = Module(new BankedStore(params))
  // 我感觉这个就是用来放secondary请求的？就是说有请求就从总线上拿下来放这里？
  // 它这儿的queue的数量是3 * mshrs，这个是有啥讲究吗？
  // secondary比mshr要多？
  val requests = Module(new ListBufferLite(ListBufferParameters(new QueuedRequest(params), 3*params.mshrs, params.secondary, false)))
  val mshrs = Seq.fill(params.mshrs) { Module(new MSHR(params)) }
  // 这是是个啥鬼东西啊？
  // init是得到列表中除了最后一项以外的其他所有项
  // abc mshrs是除了倒数两项之外的所有项
  val abc_mshrs = mshrs.init.init
  // bc mshr是倒数第二项
  val bc_mshr = mshrs.init.last
  // c mshr是倒数第一项
  val c_mshr = mshrs.last
  val nestedwb = Wire(new NestedWriteback(params))

  // connect prefetcher feedback ports
  // Acquire
  val prefetcherAcquire_arb = Module(new Arbiter(new PrefetcherAcquire(params.inner.bundle.addressBits), params.mshrs))
  io.prefetcher.acquire.valid := prefetcherAcquire_arb.io.out.valid
  io.prefetcher.acquire.bits  := prefetcherAcquire_arb.io.out.bits
  prefetcherAcquire_arb.io.out.ready := true.B
  mshrs.zipWithIndex.foreach { case (m, i) =>
    prefetcherAcquire_arb.io.in(i).valid := RegNext(m.io.prefetcherAcquire.valid)
    prefetcherAcquire_arb.io.in(i).bits  := RegNext(m.io.prefetcherAcquire.bits)
  }

  val validVec = Vec(mshrs map { case m => m.io.prefetcherAcquire.valid })
  // assert(PopCount(validVec) <= 1.U)

  // Release
  val releaseReq = sourceC.io.req.bits
  // if we drop this block, we should inform prefetcher
  // we can not catch silent drop of B-state block
  io.prefetcher.release.valid   := sourceC.io.req.fire() && (releaseReq.param === TLPermissions.BtoN || releaseReq.param === TLPermissions.TtoN)
  io.prefetcher.release.bits.address := params.expandAddress(releaseReq.tag, releaseReq.set, UInt(0))

  // connect MSHR performance counters
  val nGet     = PopCount(Vec(mshrs map { case m => m.io.mshrPerformanceCounters.valid && m.io.mshrPerformanceCounters.bits.get}))
  val nGetMiss = PopCount(Vec(mshrs map { case m => m.io.mshrPerformanceCounters.valid && m.io.mshrPerformanceCounters.bits.get && m.io.mshrPerformanceCounters.bits.miss}))
  val nPut     = PopCount(Vec(mshrs map { case m => m.io.mshrPerformanceCounters.valid && m.io.mshrPerformanceCounters.bits.put}))
  val nPutMiss = PopCount(Vec(mshrs map { case m => m.io.mshrPerformanceCounters.valid && m.io.mshrPerformanceCounters.bits.put && m.io.mshrPerformanceCounters.bits.miss}))
  val nHint     = PopCount(Vec(mshrs map { case m => m.io.mshrPerformanceCounters.valid && m.io.mshrPerformanceCounters.bits.hint}))
  val nHintMiss = PopCount(Vec(mshrs map { case m => m.io.mshrPerformanceCounters.valid && m.io.mshrPerformanceCounters.bits.hint && m.io.mshrPerformanceCounters.bits.miss}))
  val nAcquire     = PopCount(Vec(mshrs map { case m => m.io.mshrPerformanceCounters.valid && m.io.mshrPerformanceCounters.bits.acquire}))
  val nAcquireMiss = PopCount(Vec(mshrs map { case m => m.io.mshrPerformanceCounters.valid && m.io.mshrPerformanceCounters.bits.acquire && m.io.mshrPerformanceCounters.bits.miss}))
  XSPerfAccumulate(params, "nGet", nGet)
  XSPerfAccumulate(params, "nGetMiss", nGetMiss)
  XSPerfAccumulate(params, "nPut", nPut)
  XSPerfAccumulate(params, "nPutMiss", nPutMiss)
  XSPerfAccumulate(params, "nHint", nHint)
  XSPerfAccumulate(params, "nHintMiss", nHintMiss)
  XSPerfAccumulate(params, "nAcquire", nAcquire)
  XSPerfAccumulate(params, "nAcquireMiss", nAcquireMiss)

  val nActiveMSHR = PopCount(Vec(mshrs map { case m => m.io.status.valid}))
  XSPerfHistogram(params, "nActiveMSHR", nActiveMSHR, Bool(true), 0, params.mshrs, 1)

  // assert(PopCount(validVec) <= 1.U)

  // 这边的valid怎么就直接过来了呢？
  // Deliver messages from Sinks to MSHRs
  // 按照tilelink手册的描述，A和C通道的请求是按照地址来的，所以这边直接就按照set来分了？可能不是这个原因
  // 然后sinkD和E是是直接拿i做的
  mshrs.zipWithIndex.foreach { case (m, i) =>
    m.io.sinkc.valid := sinkC.io.resp.valid && sinkC.io.resp.bits.set === m.io.status.bits.set
    m.io.sinkd.valid := TrackWire(sinkD.io.resp.valid && sinkD.io.resp.bits.source === UInt(i))
    m.io.sinke.valid := sinkE.io.resp.valid && sinkE.io.resp.bits.sink   === UInt(i)
    m.io.sinkc.bits := sinkC.io.resp.bits
    m.io.sinkd.bits := sinkD.io.resp.bits
    m.io.sinke.bits := sinkE.io.resp.bits
    m.io.nestedwb := nestedwb
    m.io.mshr_id  := i.U
  }

  // 这边的mshr好像分成了抢占mshr还有普通mshr？
  // If the pre-emption BC or C MSHR have a matching set, the normal MSHR must be blocked
  // 看bc和c是否和abc match
  // 这边的stall只考虑后面的是否需要stall住前面的，而并没有考虑说前面的是不是valid的，是否真的被stall住了
  val mshr_stall_abc = abc_mshrs.map { m =>
    (bc_mshr.io.status.valid && m.io.status.bits.set === bc_mshr.io.status.bits.set) ||
    ( c_mshr.io.status.valid && m.io.status.bits.set ===  c_mshr.io.status.bits.set)
  }
  // c是否stall bc
  val mshr_stall_bc =
    c_mshr.io.status.valid && bc_mshr.io.status.bits.set === c_mshr.io.status.bits.set
  val mshr_stall_c = Bool(false)
  val mshr_stall = mshr_stall_abc :+ mshr_stall_bc :+ mshr_stall_c

  // 总的来说，似乎是普通mshr最低，第二高，c最高


  // abc是否要被stall
  val stall_abc = (mshr_stall_abc zip abc_mshrs) map { case (s, m) => s && m.io.status.valid }
  if (!params.lastLevel || !params.firstLevel)
    params.ccover(stall_abc.reduce(_||_), "SCHEDULER_ABC_INTERLOCK", "ABC MSHR interlocked due to pre-emption")
  if (!params.lastLevel)
    params.ccover(mshr_stall_bc && bc_mshr.io.status.valid, "SCHEDULER_BC_INTERLOCK", "BC MSHR interlocked due to pre-emption")

  // Consider scheduling an MSHR only if all the resources it requires are available
  // Cat过的都需要reverse一下
  // 这个是说schedule valid，并且没有被stall住，并且所有的发请求的口都ready？
  // 只要是这个的，肯定就是可以发送request的？
  val mshr_request_select = Cat((mshrs zip mshr_stall).zipWithIndex.map { case ((m, s), i) =>
    val sinkc_valid = sinkC.io.resp.valid && sinkC.io.resp.bits.set === m.io.status.bits.set
    val sinkd_valid = sinkD.io.resp.valid && sinkD.io.resp.bits.source === UInt(i)
    val sinke_valid = sinkE.io.resp.valid && sinkE.io.resp.bits.sink   === UInt(i)

    m.io.schedule.valid && !s &&
      (sourceA.io.req.ready || !m.io.schedule.bits.a.valid) &&
      (sourceB.io.req.ready || !m.io.schedule.bits.b.valid) &&
      (sourceC.io.req.ready || !m.io.schedule.bits.c.valid) &&
      (sourceD.io.req.ready || !m.io.schedule.bits.d.valid) &&
      (sourceE.io.req.ready || !m.io.schedule.bits.e.valid) &&
      (sourceX.io.req.ready || !m.io.schedule.bits.x.valid) &&
      (directory.io.write.ready || !m.io.schedule.bits.dir.valid) &&
      TrackWire(!(sinkc_valid || sinkd_valid || sinke_valid))  // these valid can be active in s_select and change status compared to schedule_issue
  }.reverse)

  // Round-robin arbitration of MSHRs
  // 这边是mshr的请求发出去，但是只能发送一个？why？这么多channel不是应该能并行地发送吗？
  val CONFIG_ROBIN = false

  val robin_filter = RegInit(UInt(0, width = params.mshrs))
  val robin_request = if (CONFIG_ROBIN) Cat(mshr_request_select, mshr_request_select & robin_filter) else mshr_request_select
  val mshr_selectOH2 = TrackWire((~(leftOR(robin_request) << 1)).asUInt & robin_request)
  val mshr_selectOH_select: UInt = if (CONFIG_ROBIN) mshr_selectOH2(2*params.mshrs-1, params.mshrs) | mshr_selectOH2(params.mshrs-1, 0)
                            else mshr_selectOH2(params.mshrs - 1, 0)  // This bitsect is necessary to remove msb.
  // 这个是这波儿选出的request
  val mshr_select = OHToUInt(mshr_selectOH_select)

  val schedule_select = Mux1H(mshr_selectOH_select, mshrs.map(_.io.schedule.bits))
  val scheduleStatus_select = Mux1H(mshr_selectOH_select, mshrs.map(_.io.status.bits))

  val cut = Module(new ScheduleCut(params))
  val s_select = cut.io.s_select
  val s_issue = cut.io.s_issue
  sourceC.io.s_select := s_select

  val source_readys_curr = Cat(
    sourceA.io.req.ready,
    sourceB.io.req.ready,
    sourceC.io.req.ready,
    sourceD.io.req.ready,
    sourceE.io.req.ready,
    sourceX.io.req.ready,
    directory.io.write.ready)
  val schedule_select_reqs = Cat(
    schedule_select.a.valid,
    schedule_select.b.valid,
    schedule_select.c.valid,
    schedule_select.d.valid,
    schedule_select.e.valid,
    schedule_select.x.valid,
    schedule_select.dir.valid)
  val schedule_issue_reqs = Cat(
    cut.io.issue.schedule.a.valid,
    cut.io.issue.schedule.b.valid,
    cut.io.issue.schedule.c.valid,
    cut.io.issue.schedule.d.valid,
    cut.io.issue.schedule.e.valid,
    cut.io.issue.schedule.x.valid,
    cut.io.issue.schedule.dir.valid)
  val select_fire_prev = RegNext(source_readys_curr & schedule_select_reqs)

  when (s_issue) {
    assert(select_fire_prev === (source_readys_curr & schedule_issue_reqs), "schedule fire changed, expect fire %b, curr ready %b, reqs %b, selectOH %b, need %b",
      select_fire_prev,
      source_readys_curr,
      schedule_issue_reqs,
      cut.io.issue.mshrSelectOH,
      RegNext(cut.io.need_to_schedule))
  }

  cut.io.need_to_schedule := TrackWire(mshr_request_select.orR())
  cut.io.select.mshrReqValids := TrackWire(mshr_request_select)
  cut.io.select.mshrStatus := scheduleStatus_select
  cut.io.select.mshrSelectOH := TrackWire(mshr_selectOH_select)
  cut.io.select.schedule := TrackWire(schedule_select)


  // When an MSHR wins the schedule, it has lowest priority next time
  when (s_issue && cut.io.issue.mshrReqValids.orR()) {
    robin_filter := ~rightOR(cut.io.issue.mshrSelectOH)
  }

  // Fill in which MSHR sends the request
  schedule_select.a.bits.source := mshr_select
  // 这个是啥意思？ReleaseData和PorobeAckData有啥差别吗？
  // Release[Data]是6 7， ProbeAck是4 5
  // 所以release时id是选mshr
  // 如果是probeAck，就是零，这是为啥呢？
  schedule_select.c.bits.source := Mux(schedule_select.c.bits.opcode(1), mshr_select, UInt(0)) // only set for Release[Data] not ProbeAck[Data]
  schedule_select.d.bits.sink   := mshr_select

  val schedule_issue = cut.io.issue.schedule
  sourceA.io.req := schedule_issue.a
  sourceB.io.req := schedule_issue.b
  sourceC.io.req := schedule_issue.c
  sourceD.io.req := schedule_issue.d
  sourceE.io.req := schedule_issue.e
  sourceX.io.req := schedule_issue.x
  directory.io.write := schedule_issue.dir

  when (sourceC.io.req.fire()) {
    DebugPrint(params, "sourceC.fire mshr %d\n", RegNext(mshr_select))
  }

  // nested transaction completion是啥？
  // Forward meta-data changes from nested transaction completion
  // 如果是C或者BC的schedule，那么其他基本模块儿的meta data可能得跟着改改
  // 选中的是不是最后一个通道的C mshr
  val select_c  = cut.io.issue.mshrSelectOH(params.mshrs-1)
  // 选中的是不是倒数第二个通道的BC mshr
  val select_bc = cut.io.issue.mshrSelectOH(params.mshrs-2)
  nestedwb.set   := Mux(select_c, c_mshr.io.status.bits.set, bc_mshr.io.status.bits.set)
  nestedwb.tag   := Mux(select_c, c_mshr.io.status.bits.tag, bc_mshr.io.status.bits.tag)
  // 这边是要将state改成N或者B
  nestedwb.b_toN       := select_bc && bc_mshr.io.schedule.bits.dir.valid && bc_mshr.io.schedule.bits.dir.bits.data.state === MetaData.INVALID
  nestedwb.b_toB       := select_bc && bc_mshr.io.schedule.bits.dir.valid && bc_mshr.io.schedule.bits.dir.bits.data.state === MetaData.BRANCH
  // 这个是clear dirty
  nestedwb.b_clr_dirty := select_bc && bc_mshr.io.schedule.bits.dir.valid
  // set dirty
  // 这些都是啥？
  nestedwb.c_set_dirty := select_c  &&  c_mshr.io.schedule.bits.dir.valid && c_mshr.io.schedule.bits.dir.bits.data.dirty

  // Pick highest priority request
  // 这里才是真正的将request分发到mshr
  val request = Wire(Decoupled(new FullRequest(params)))
  // request只有在directory valid时才有效？
  // 所以有可能是进mshr时，把directory给读了？
  // 需要单独分配mshr的请求有inner A，X，还有C
  // 我感觉对外如果支持一致性的话，或许得把sinkB也搞到这里来？
  request.valid := directory.io.ready && (sinkA.io.req.valid || sinkX.io.req.valid || sinkB.io.req.valid || sinkC.io.req.valid)
  request.bits := Mux(sinkC.io.req.valid, sinkC.io.req.bits,
                  Mux(sinkB.io.req.valid, sinkB.io.req.bits,
                  Mux(sinkX.io.req.valid, sinkX.io.req.bits, sinkA.io.req.bits)))
  // 这边的几个sink可能会同时valid，然后这里的优先级是C最高，X次之，A最低，这是为啥呢？
  sinkC.io.req.ready := directory.io.ready && request.ready
  sinkB.io.req.ready := directory.io.ready && request.ready && !sinkC.io.req.valid
  sinkX.io.req.ready := directory.io.ready && request.ready && !sinkC.io.req.valid && !sinkB.io.req.valid
  sinkA.io.req.ready := directory.io.ready && request.ready && !sinkC.io.req.valid && !sinkB.io.req.valid && !sinkX.io.req.valid

  // If no MSHR has been assigned to this set, we need to allocate one
  val request_set = TrackWire(request.bits.set)
  val setMatches = Cat(mshrs.map { m => m.io.status.valid && m.io.status.bits.set === request_set }.reverse)
  // 没有set match的
  val alloc = !setMatches.orR() // NOTE: no matches also means no BC or C pre-emption on this set
  // If a same-set MSHR says that requests of this type must be blocked (for bounded time), do it
  // 这个block的意思是，现在要把这个给block住
  // prio是啥呢？
  // 如果某一个和request set match，那就把blockB、C选出来
  // prio 0 1 2都是什么鬼东西？
  // 主要是这里的prio是啥东西啊？
  // 现在prio 2的是C
  // 还没有prio是1的，1估计就是B
  // 估计就是各个请求根据prio的bit，来标识是acquire，probe还是release
  // 毕竟主体流程其实就三种
  // 每个人可以喝说自己这个mshr是否要block
  // 估计prio就是priority？
  // X和A显然是最低的，B次之，C最高？
  // 当setMatches真的有match时，blockB和blockC的值才是defined，假如没有match，值就是not defined
  // 在那种情况下，是alloc，是分配普通的MSHR。不会分配特殊的MSHR。
  val blockB = Mux1H(setMatches, mshrs.map(_.io.status.bits.blockB)) && request.bits.prio(1)
  val blockC = Mux1H(setMatches, mshrs.map(_.io.status.bits.blockC)) && request.bits.prio(2)

  // If a same-set MSHR says that requests of this type must be handled out-of-band, use special BC|C MSHR
  // ... these special MSHRs interlock the MSHR that said it should be pre-empted.
  // nest是现在可以允许处理这个请求，但是在处理这个请求的时候，自己是被堵住的
  // 估计是每个mshr可以说自己这个是否要nest？
  // 估计假如不支持nest，就要另外分配mshr了？
  val nestB  = Mux1H(setMatches, mshrs.map(_.io.status.bits.nestB))  && request.bits.prio(1)
  val nestC  = Mux1H(setMatches, mshrs.map(_.io.status.bits.nestC))  && request.bits.prio(2)
  // Prevent priority inversion; we may not queue to MSHRs beyond our level
  // priority inversion到底是个什么问题？
  // 这尼玛写错了吧？中间的为啥是零，不是应该是bc吗？
  // 低位的全部是1
  val prioFilterx = Cat(request.bits.prio(2), !request.bits.prio(0), ~UInt(0, width = params.mshrs-2))
  val prioFilter  = Cat(sinkC.io.req.valid, sinkB.io.req.valid || sinkC.io.req.valid, ~UInt(0, width = params.mshrs-2))
  assert(!request.valid || (prioFilter === prioFilterx), "prio %b, %b should be correspond to sink valids", prioFilter, prioFilterx)

  val lowerMatches = setMatches & prioFilter
  // 如果这个request暂时没法处理，那就放进buffer
  // 这边是确定要不要queue？
  // 假如不是queue，那去哪儿呢？
  // If we match an MSHR <= our priority that neither blocks nor nests us, queue to it.
  // MSHR的priority是啥？估计就是channel的priority？
  // 仔细分析一下tilelink的请求流程主要就三个：
  // 1. acquire
  // 2. probe
  // 3. release
  // 其他几个channel都是之前channel的response，是从属于之前的channel的，不需要额外分配资源，不需要阻塞。
  // 只有A B C这几个请求开启流程是需要额外分配资源的，所以也就因此有优先级要遵守？
  // A请求不能阻塞B，是显然的，典型的例子就是
  // 可能blockB，nestC的意思是，它们现在支持B或者C进来？
  // block和nest估计是低位有效，还有问题就是当nest时，inter lock是怎样lock的呢？
  // 如果支持nestB或者nestC，那显然就不用暂时queue起来了
  // 如果blockB是true，就意味着有人要block它。
  // 为什么有人要block它，它也进不了queue呢？
  // 那估计这边的block就是挂在总线上不拿下来的意思？
  val queue = lowerMatches.orR() && !nestB && !nestC && !blockB && !blockC

  if (!params.lastLevel) {
    params.ccover(request.valid && blockB, "SCHEDULER_BLOCKB", "Interlock B request while resolving set conflict")
    params.ccover(request.valid && nestB,  "SCHEDULER_NESTB", "Priority escalation from channel B")
  }
  if (!params.firstLevel) {
    params.ccover(request.valid && blockC, "SCHEDULER_BLOCKC", "Interlock C request while resolving set conflict")
    params.ccover(request.valid && nestC,  "SCHEDULER_NESTC", "Priority escalation from channel C")
  }
  params.ccover(request.valid && queue, "SCHEDULER_SECONDARY", "Enqueue secondary miss")

  // 下面这一拨儿应该就是进队以及出队的管理

  // It might happen that lowerMatches has >1 bit if the two special MSHRs are in-use
  // We want to Q to the highest matching priority MSHR.
  val lowerMatches1 =
    Mux(lowerMatches(params.mshrs-1), UInt(1 << (params.mshrs-1)),
    Mux(lowerMatches(params.mshrs-2), UInt(1 << (params.mshrs-2)),
    lowerMatches))

  // 如果这个新请求是进之前被schedule的mshr？
  // If this goes to the scheduled MSHR, it may need to be bypassed
  // Alternatively, the MSHR may be refilled from a request queued in the ListBuffer
  // 要和可能选择是从之前的队列里面给pop一项出来
  val mshr_selectOH_issue = cut.io.issue.mshrSelectOH
  val selected_requests = Cat(mshr_selectOH_issue, mshr_selectOH_issue, mshr_selectOH_issue) & requests.io.valid
  val a_pop = selected_requests((0 + 1) * params.mshrs - 1, 0 * params.mshrs).orR()
  val b_pop = selected_requests((1 + 1) * params.mshrs - 1, 1 * params.mshrs).orR()
  val c_pop = selected_requests((2 + 1) * params.mshrs - 1, 2 * params.mshrs).orR()
  val bypassMatches = (mshr_selectOH_issue & lowerMatches1).orR() &&
                      Mux(c_pop || request.bits.prio(2), !c_pop, Mux(b_pop || request.bits.prio(1), !b_pop, !a_pop))
  val may_pop = a_pop || b_pop || c_pop
  val bypass = request.valid && queue && bypassMatches
  val will_reload = schedule_issue.reload && (may_pop || bypass)
  val will_pop = schedule_issue.reload && may_pop && !bypass

  params.ccover(mshr_selectOH_issue.orR && bypass, "SCHEDULER_BYPASS", "Bypass new request directly to conflicting MSHR")
  params.ccover(mshr_selectOH_issue.orR && will_reload, "SCHEDULER_RELOAD", "Back-to-back service of two requests")
  params.ccover(mshr_selectOH_issue.orR && will_pop, "SCHEDULER_POP", "Service of a secondary miss")

  cut.io.select.prioRequests := ~((~requests.io.valid).asUInt | (requests.io.valid >> params.mshrs).asUInt | (requests.io.valid >> 2*params.mshrs).asUInt)
  val prio_requests_issue = cut.io.issue.prioRequests
  val pop_onehot = Cat(mshr_selectOH_issue, mshr_selectOH_issue, mshr_selectOH_issue) & prio_requests_issue
  // Determine which of the queued requests to pop (supposing will_pop)
  val pop_index = OHToUInt(pop_onehot)
  requests.io.pop.valid := will_pop
  requests.io.pop.bits  := pop_index
  requests.io.pop_onehot_index := pop_onehot

  // 这里其实就是，请求要么是从外面来的，要么是从队列里面选出来的
  // Repeat the above logic, but without the fan-in
  mshrs.zipWithIndex.foreach { case (m, i) =>
    val sel = mshr_selectOH_issue(i)
    assert(!(s_select && sel))
    m.io.schedule.ready := sel
    val a_pop = requests.io.valid(params.mshrs * 0 + i)
    val b_pop = requests.io.valid(params.mshrs * 1 + i)
    val c_pop = requests.io.valid(params.mshrs * 2 + i)
    val bypassMatches = lowerMatches1(i) &&
                        Mux(c_pop || request.bits.prio(2), !c_pop, Mux(b_pop || request.bits.prio(1), !b_pop, !a_pop))
    val may_pop = a_pop || b_pop || c_pop
    val bypass = request.valid && queue && bypassMatches
    val will_reload = m.io.schedule.bits.reload && (may_pop || bypass)
    m.io.allocate.bits := Mux(bypass, Wire(new QueuedRequest(params), init = request.bits), requests.io.data)
    m.io.allocate.bits.set := m.io.status.bits.set

    when (sel) {
      assert(schedule_issue.reload === m.io.schedule.bits.reload, "s_issue atomicity broke")
    }
    val bypass_repeat = m.io.status.bits.tag === request.bits.tag
    val schedule_repeat_abc = Seq(0, 1, 2)
      .map { t => requests.io.dataFanout(params.mshrs * t + i).tag === m.io.status.bits.tag }
    val select_abc = Seq(0, 1, 2).map { t => pop_onehot(params.mshrs * t + i) }
    val schedule_repeat = (schedule_repeat_abc & select_abc).orR

    //m.io.allocate.bits.repeat := m.io.allocate.bits.tag === m.io.status.bits.tag
    m.io.allocate.bits.repeat := false.B
    m.io.allocate.valid := sel && will_reload && s_issue

    when (m.io.allocate.valid && bypass) {
      assert(request.ready)
    }

    when (sel) {
      DebugPrint(params, "mshr %d allocate a_pop %b b_pop %b c_pop %b bypassMatches %b bypass %b bypass_repeat %b schedule_repeat %b\n", i.U, a_pop, b_pop, c_pop, bypassMatches, bypass, bypass_repeat, schedule_repeat)
    }
  }

  // Reload from the Directory if the next MSHR operation changes tags
  val scheduleTag_issue = cut.io.issue.mshrStatus.tag
  val scheduleSet_issue = cut.io.issue.mshrStatus.set
  // val scheduleMatches = (requests.io.dataFanout.map(_.tag =/= scheduleTag_issue) & pop_onehot.toBools).orR
  val scheduleMatches = requests.io.data.tag =/= scheduleTag_issue
  val scheduleMatchesReal = Mux(bypass, scheduleTag_issue =/= request.bits.tag, scheduleMatches)
  val lb_tag_mismatch = scheduleMatches
  val mshr_uses_directory_assuming_no_bypass = schedule_issue.reload && may_pop
  val mshr_uses_directory_for_lb = will_pop
  val mshr_uses_directory = will_reload

  // Is there an MSHR free for this request?
  val mshr_validOH = Cat(mshrs.map(_.io.status.valid).reverse)
  val mshr_free = ((~mshr_validOH).asUInt & prioFilter).orR()

  // Fanout the request to the appropriate handler (if any)
  val bypassQueue = schedule_issue.reload && bypassMatches
  val request_alloc_cases =
     (alloc && !mshr_uses_directory_assuming_no_bypass && mshr_free) ||
     (nestB && !mshr_uses_directory_assuming_no_bypass && !bc_mshr.io.status.valid && !c_mshr.io.status.valid) ||
     (nestC && !mshr_uses_directory_assuming_no_bypass && !c_mshr.io.status.valid)

  val can_bypass_in_issue = schedule_select.reload && bypassMatches
  request.ready := false.B
  when (s_select) {
    request.ready := request_alloc_cases
  }
  when (s_issue) {
    DebugPrint(params, "request_alloc_cases %b queue %b bypassQueue %b\n", request_alloc_cases, queue, bypassQueue)
    request.ready := request_alloc_cases || (queue && (bypassQueue || requests.io.push.ready))
  }

  val alloc_uses_directory = request.valid && request_alloc_cases

  // When a request goes through, it will need to hit the Directory
  directory.io.read.valid := mshr_uses_directory || alloc_uses_directory
  directory.io.read.bits.set := Mux(mshr_uses_directory_for_lb, scheduleSet_issue,    request.bits.set)
  directory.io.read.bits.tag := Mux(mshr_uses_directory_for_lb, requests.io.data.tag, request.bits.tag)

  // Enqueue the request if not bypassed directly into an MSHR
  requests.io.push.valid := request.valid && queue && (s_issue && !bypassQueue)
  when (requests.io.push.fire()) {
    assert(request.fire())
    DebugPrint(params, "requests push for mshr %d prio %b\n", OHToUInt(lowerMatches1), request.bits.prio.asUInt())
  }
  requests.io.push.bits.data  := request.bits
  requests.io.push.bits.index := Mux1H(
    request.bits.prio, Seq(
      OHToUInt(TrackWire(lowerMatches1) << params.mshrs*0),
      OHToUInt(TrackWire(lowerMatches1) << params.mshrs*1),
      OHToUInt(TrackWire(lowerMatches1) << params.mshrs*2)))
  requests.io.push_onehot_index := Mux1H(
    request.bits.prio, Seq(
      lowerMatches1 << params.mshrs*0,
      lowerMatches1 << params.mshrs*1,
      lowerMatches1 << params.mshrs*2
    )
  )

  val mshr_insertOH: UInt = TrackWire(~(leftOR(~mshr_validOH) << 1) & ~mshr_validOH & prioFilter)
  (mshr_insertOH.asBools zip mshrs) map { case (s, m) =>
    when (TrackWire(request.valid) && TrackWire(alloc) && s && TrackWire(!mshr_uses_directory_assuming_no_bypass)) {
      m.io.allocate.valid := Bool(true)
      m.io.allocate.bits := request.bits
      m.io.allocate.bits.repeat := Bool(false)

      when (m.io.allocate.valid) {
        assert(request.ready)
        DebugPrint(params, "MSHR %d: insert req\n", m.io.mshr_id)
      }
    }
  }

  when (request.valid && nestB && !bc_mshr.io.status.valid && !c_mshr.io.status.valid && !mshr_uses_directory_assuming_no_bypass) {
    bc_mshr.io.allocate.valid := Bool(true)
    bc_mshr.io.allocate.bits := request.bits
    bc_mshr.io.allocate.bits.repeat := Bool(false)
    assert (!request.bits.prio(0))
    assert(request.ready)
    when (bc_mshr.io.allocate.valid) {
      DebugPrint(params, "MSHR %d: (bc) insert req\n", bc_mshr.io.mshr_id)
    }
  }
  bc_mshr.io.allocate.bits.prio(0) := Bool(false)

  when (request.valid && nestC && !c_mshr.io.status.valid && !mshr_uses_directory_assuming_no_bypass) {
    c_mshr.io.allocate.valid := Bool(true)
    c_mshr.io.allocate.bits := request.bits
    c_mshr.io.allocate.bits.repeat := Bool(false)
    assert (!request.bits.prio(0))
    assert (!request.bits.prio(1))
    assert(request.ready)
    when (c_mshr.io.allocate.valid) {
      DebugPrint(params, "MSHR %d: (c) insert req\n", c_mshr.io.mshr_id)
    }
  }
  c_mshr.io.allocate.bits.prio(0) := Bool(false)
  c_mshr.io.allocate.bits.prio(1) := Bool(false)

  // Fanout the result of the Directory lookup
  val dirTarget = Mux(alloc, mshr_insertOH, Mux(nestB, UInt(1 << (params.mshrs-2)), UInt(1 << (params.mshrs-1))))
  val directoryFanout = params.dirReg(RegNext(RegNext(Mux(mshr_uses_directory, mshr_selectOH_issue, Mux(alloc_uses_directory, dirTarget, UInt(0))))))
  mshrs.zipWithIndex.foreach { case (m, i) =>
    m.io.directory.valid := directoryFanout(i)
    m.io.directory.bits := RegNext(directory.io.result.bits)
  }

  // MSHR response meta-data fetch
  sinkC.io.way :=
    Mux(bc_mshr.io.status.valid && bc_mshr.io.status.bits.set === sinkC.io.set,
      bc_mshr.io.status.bits.way,
      Mux1H(abc_mshrs.map(m => m.io.status.valid && m.io.status.bits.set === sinkC.io.set),
            abc_mshrs.map(_.io.status.bits.way)))
  sinkD.io.way := Vec(mshrs.map(_.io.status.bits.way))(sinkD.io.source)
  sinkD.io.set := Vec(mshrs.map(_.io.status.bits.set))(sinkD.io.source)

  // Beat buffer connections between components
  sinkA.io.pb_pop <> sourceD.io.pb_pop
  sourceD.io.pb_beat := sinkA.io.pb_beat
  sinkD.io.gb_pop <> sourceD.io.gnt_pop
  sourceD.io.gnt_beat := sinkD.io.gb_beat
  sinkC.io.rel_pop <> sourceD.io.rel_pop
  sourceD.io.rel_beat := sinkC.io.rel_beat

  // BankedStore ports
  bankedStore.io.sinkC_adr <> sinkC.io.bs_adr
  bankedStore.io.sinkC_dat := sinkC.io.bs_dat
  bankedStore.io.sinkD_adr <> sinkD.io.bs_adr
  bankedStore.io.sinkD_dat := sinkD.io.bs_dat
  bankedStore.io.sourceC_adr <> sourceC.io.bs_adr
  bankedStore.io.sourceD_radr <> sourceD.io.bs_radr
  bankedStore.io.sourceD_wadr <> sourceD.io.bs_wadr
  bankedStore.io.sourceD_wdat := sourceD.io.bs_wdat
  sourceC.io.bs_dat := bankedStore.io.sourceC_dat
  sourceD.io.bs_rdat := bankedStore.io.sourceD_rdat

  // SourceD data hazard interlock
  sourceD.io.evict_req := sourceC.io.evict_req
  sourceD.io.grant_req := sinkD  .io.grant_req
  sourceC.io.evict_safe := sourceD.io.evict_safe
  sinkD  .io.grant_safe := sourceD.io.grant_safe

  val setConflict = requests.io.push.fire()
  val mshrUseBypass = mshr_selectOH_issue.orR() && bypass
  XSPerfAccumulate(params, "nSetConflict", setConflict)
  XSPerfAccumulate(params, "nMSHRBypass", mshrUseBypass)

  private def afmt(x: AddressSet) = s"""{"base":${x.base},"mask":${x.mask}}"""
  private def addresses = params.inner.manager.managers.flatMap(_.address).map(afmt _).mkString(",")
  private def setBits = params.addressMapping.drop(params.offsetBits).take(params.setBits).mkString(",")
  private def tagBits = params.addressMapping.drop(params.offsetBits + params.setBits).take(params.tagBits).mkString(",")
  private def simple = s""""reset":"${reset.pathName}","tagBits":[${tagBits}],"setBits":[${setBits}],"blockBytes":${params.cache.blockBytes},"ways":${params.cache.ways}"""
}
