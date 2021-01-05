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
import freechips.rocketchip.tilelink._
import TLMessages.{AccessAckData}
import freechips.rocketchip.util._

class GrantBufferDEntry(params: InclusiveCacheParameters) extends InclusiveCacheBundle(params)
{
  val data = UInt(width = params.inner.bundle.dataBits)
  val param = UInt(width = 3)
  def dump() = {
    DebugPrint(params, "GrantBufferDEntry: data: %x param: %x\n",
      data, param)
  }
}

// Instead of using ListBuffer just as putBuffer and releaseBuffer does,
// we implemented a new grant buffer.
// With this buffer, sourceD can choose which beat to use,
// instead of consuming one by one.
// Suitable for sub-block level get
class GrantBufferAllocate(params: InclusiveCacheParameters) extends InclusiveCacheBundle(params)
{
  val index = UInt(width = params.putBits)
  def dump() = {
    DebugPrint(params, "GrantBufferAllocate: index: %d\n", index)
  }
}

class GrantBufferPush(params: InclusiveCacheParameters) extends InclusiveCacheBundle(params)
{
  val nBeats = params.cache.blockBytes * 8 / params.inner.bundle.dataBits
  val index = UInt(width = params.putBits)
  val beat = UInt(width = log2Up(nBeats))
  val data = UInt(width = params.inner.bundle.dataBits)
  val param = UInt(width = 3)
  def dump() = {
    DebugPrint(params, "GrantBufferPush: index: %d beat: %d data: %x param: %x\n",
      index, beat, data, param)
  }
}

class GrantBufferPop(params: InclusiveCacheParameters) extends InclusiveCacheBundle(params)
{
  val nBeats = params.cache.blockBytes * 8 / params.inner.bundle.dataBits

  val index = UInt(width = params.putBits)
  val beat = UInt(width = log2Up(nBeats))
  val last = Bool()
  def dump() = {
    DebugPrint(params, "GrantBufferPop: index: %d beat: %d last: %b\n",
      index, beat, last)
  }
}

class GrantBuffer(params: InclusiveCacheParameters) extends Module
{
  val io = new Bundle {
    val allocate = Decoupled(new GrantBufferAllocate(params))
    val push = Decoupled(new GrantBufferPush(params)).flip
    val pop = Decoupled(new GrantBufferPop(params)).flip
    val beat = new GrantBufferDEntry(params)
  }
  val nBeats = params.cache.blockBytes * 8 / params.inner.bundle.dataBits
  val buffer = Reg(Vec(params.grantLists, Vec(nBeats, UInt(width = params.inner.bundle.dataBits))))
  val param = Reg(Vec(params.grantLists, Vec(nBeats, UInt(width = 3))))

  // list allocation and free
  val lists = RegInit(UInt(0, width = params.grantLists))
  val lists_set = Wire(init = UInt(0, width = params.grantLists))
  val lists_clr = Wire(init = UInt(0, width = params.grantLists))
  lists := (lists | lists_set) & ~lists_clr

  val free = !lists.andR()
  val freeOH = ~(leftOR(~lists) << 1) & ~lists
  val freeIdx = OHToUInt(freeOH)

  io.allocate.valid := free
  io.allocate.bits.index := freeIdx

  when (io.allocate.fire()) {
    lists_set := freeOH
  }

  // list free
  when (io.pop.fire() && io.pop.bits.last) {
    lists_clr := UIntToOH(io.pop.bits.index, params.grantLists)
  }

  // push data beat
  val push = io.push.bits
  io.push.ready := true.B
  when (io.push.fire()) {
    buffer(push.index)(push.beat) := push.data
    param(push.index)(push.beat) := push.param
  }

  // pop data beat
  val pop = io.pop.bits
  io.pop.ready := true.B
  io.beat.data := 0.U
  io.beat.param := 0.U
  when (io.pop.fire()) {
    io.beat.data := buffer(pop.index)(pop.beat)
    io.beat.param := param(pop.index)(pop.beat)
  }

  // dump
  when (io.allocate.fire()) {
    io.allocate.bits.dump()
  }

  when (io.push.fire()) {
    io.push.bits.dump()
  }

  when (io.pop.fire()) {
    io.pop.bits.dump()
    io.beat.dump()
  }
}

class SinkDResponse(params: InclusiveCacheParameters) extends InclusiveCacheBundle(params)
{
  val last   = Bool()
  val opcode = UInt(width = 3)
  val param  = UInt(width = 3)
  val source = UInt(width = params.outer.bundle.sourceBits)
  val sink   = UInt(width = params.outer.bundle.sinkBits)
  val denied = Bool()
  val grant  = UInt(width = params.putBits)
  def dump() = {
    DebugPrint(params, "SinkDResponse: last: %b opcode: %x param: %x source: %x sink: %x denied: %b grant: %d\n",
      last, opcode, param, source, sink, denied, grant)
  }
}

class SinkD(params: InclusiveCacheParameters) extends Module with HasTLDump
{
  val io = new Bundle {
    val resp = Valid(new SinkDResponse(params)) // Grant or ReleaseAck
    val d = Decoupled(new TLBundleD(params.outer.bundle)).flip
    // Lookup the set+way from MSHRs
    val source = UInt(width = params.outer.bundle.sourceBits)
    val way    = UInt(width = params.wayBits).flip
    val set    = UInt(width = params.setBits).flip
    // Banked Store port
    val bs_adr = Decoupled(new BankedStoreOuterAddress(params))
    val bs_dat = new BankedStoreOuterPoison(params)
    // WaR hazard
    val grant_req = new SourceDHazard(params)
    val grant_safe = Bool().flip

    // for use by SourceD:
    val gb_pop  = Decoupled(new GrantBufferPop(params)).flip
    val gb_beat = new GrantBufferDEntry(params)
  }

  when (io.resp.fire()) {
    DebugPrint(params, "sinkD resp ")
    io.resp.bits.dump
  }

  val innerBeatWidth = params.inner.bundle.dataBits
  val outerBeatWidth = params.outer.bundle.dataBits
  val split = innerBeatWidth / outerBeatWidth
  require (innerBeatWidth >= outerBeatWidth)

  /*
  when (io.d.fire()) {
    DebugPrint(params, "outer grant ")
    io.d.bits.dump
  }
  */

  // DebugPrint(params, "sinkD: source: %x set: %x way: %x\n", io.source, io.set, io.way)

  when (io.bs_adr.fire()) {
    DebugPrint(params, "sinkD bs_adr ")
    io.bs_adr.bits.dump
  }

  /*
  DebugPrint(params, "sinkD bs_dat ")
  io.bs_dat.dump

  DebugPrint(params, "sinkD grant_req ")
  io.grant_req.dump

  when (io.grant_safe) {
    DebugPrint(params, "sinkD grant_safe\n")
  }
  */

  when (io.gb_pop.fire()) {
    DebugPrint(params, "sinkD gb_pop ")
    io.gb_pop.bits.dump
    DebugPrint(params, "sinkD gb_beat: %x ", io.gb_beat.data)
  }


  // No restrictions on buffer
  val d = params.micro.outerBuf.d(io.d)

  val grantbuffer = Module(new GrantBuffer(params))

  val freeIdx = grantbuffer.io.allocate.bits.index
  val (first, last, _, beat) = params.outer.count(d)
  val hasData = params.outer.hasData(d.bits)
  val grant = Mux(first, freeIdx, RegEnable(freeIdx, first))

  // 常见写法，valid时直接用，不valid时，用锁存的值
  io.source := Mux(d.valid, d.bits.source, RegEnable(d.bits.source, d.valid))
  io.grant_req.way := io.way
  io.grant_req.set := io.set

  val uncache = d.bits.opcode === AccessAckData
  val cache = !uncache

  // We need to split the D input to three places:
  //   If it is the first beat, it must go to req
  //   If it belongs to an uncache and has Data, it must go to the grantbuffer
  //   If it belongs to an uncache has Data AND is the first beat, it must claim a list

  // need to block D response when grant is not safe
  val resp_block = first && !io.grant_safe
  // buf block则是要保存数据，这个只要有数据，就得保存
  val buf_block = hasData && !grantbuffer.io.push.ready
  // set block是建立保存的list，这个只有对于第一拍需要建立list
  val set_block = hasData && first && !grantbuffer.io.allocate.valid

  params.ccover(d.valid && resp_block, "SINKD_REQ_STALL", "Grant not safe for sink response")
  params.ccover(d.valid && buf_block, "SINKD_BUF_STALL", "No space in grantbuffer for beat")
  params.ccover(d.valid && set_block, "SINKD_SET_STALL", "No space in grantbuffer for response")

  val uncache_ready = !buf_block && !set_block
  val cache_ready = io.bs_adr.ready

  d.ready := !resp_block && ((cache && cache_ready) || (uncache && uncache_ready))

  // response
  // ---------------------------------------------
  // 为什么只要在first和last时给valid？
  // 为什么都要not first呢？
  // 看来response只需要给两个，一个是开始，另一个是结束
  io.resp.valid := (first || last) && d.fire()
  io.resp.bits.last   := last
  io.resp.bits.opcode := d.bits.opcode
  io.resp.bits.param  := d.bits.param
  io.resp.bits.source := d.bits.source
  io.resp.bits.sink   := d.bits.sink
  io.resp.bits.denied := d.bits.denied
  io.resp.bits.grant  := grant


  // banked store
  // ---------------------------------------------
  // 总感觉这边有点问题啊？
  // 为啥不是first就要valid呢？
  // 当不是first时，要和不看d是否valid吗？就直接写吗？这也不太对吧？我感觉这个是假设了message都是连续拍的吧？这个不一定能保证的吧？
  io.bs_adr.valid := cache && (!first || (d.valid && io.grant_safe))
  params.ccover(d.valid && first && !io.grant_safe, "SINKD_HAZARD", "Prevented Grant data hazard with backpressure")
  params.ccover(io.bs_adr.valid && !io.bs_adr.ready, "SINKD_SRAM_STALL", "Data SRAM busy")

  // 这是啥意思？
  // Also send Grant(NoData) to BS to ensure correct data ordering
  io.bs_adr.bits.noop := !d.valid || !hasData
  io.bs_adr.bits.way  := io.way
  io.bs_adr.bits.set  := io.set
  io.bs_adr.bits.beat := Mux(d.valid, beat, RegEnable(beat + io.bs_adr.ready.asUInt, d.valid))
  io.bs_adr.bits.mask := ~UInt(0, width = params.outerMaskBits)
  io.bs_dat.data      := d.bits.data

  assert (!(d.valid && d.bits.corrupt && !d.bits.denied), "Data poisoning unsupported")


  // grantBuffer
  // ---------------------------------------------
  val refill_data = Reg(Vec(split, UInt(outerBeatWidth.W)))
  val splitBits = log2Floor(split)
  val beatIndex = if (splitBits == 0) 0.U else beat(splitBits - 1, 0)
  val beatFull = if (splitBits == 0) true.B else beatIndex === (split - 1).U
  when (d.fire() && uncache && hasData) {
    refill_data(beatIndex) := d.bits.data
  }
  val fullBeat = Cat((0 until split).reverse map { i => refill_data(i) })

  grantbuffer.io.allocate.ready := d.fire() && uncache && hasData && first

  grantbuffer.io.push.valid := RegNext(next = d.fire() && uncache && hasData && beatFull, init = false.B)
  grantbuffer.io.push.bits.index := RegNext(grant)
  grantbuffer.io.push.bits.beat := RegNext(beat >> splitBits)
  grantbuffer.io.push.bits.data := fullBeat
  grantbuffer.io.push.bits.param := RegNext(d.bits.param)

  when (d.fire()) {
    DebugPrint(params, "sinkD first: %b uncache: %b hasData: %b beatFull: %b\n",
      first, uncache, hasData, beatFull)
  }

  // Grant access to pop the data
  // 当数据收集全了后，就要pop，这个数据是怎么pop出来的呢？是子写进去的吗？肯定不是，肯定还得有替换算法的。
  grantbuffer.io.pop <> io.gb_pop
  io.gb_beat <> grantbuffer.io.beat
}
