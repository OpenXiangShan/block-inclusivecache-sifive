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
import org.chipsalliance.cde.config._
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.tilelink._
import freechips.rocketchip.util.ReplacementPolicy
import MetaData._

class DirectoryEntry(params: InclusiveCacheParameters) extends InclusiveCacheBundle(params)
{
  val dirty   = Bool() // true => TRUNK or TIP
  val prefetch_hit = Bool() // true => TRUNK or TIP
  val state   = UInt(width = params.stateBits)
  val clients = UInt(width = params.clientBits)
  val tag     = UInt(width = params.tagBits)
  def dump() = {
    DebugPrint(params, "DirectoryEntry: dirty: %b state: %d clients: %x tag: %x\n",
      dirty, state, clients, tag)
  }
}

class DirectoryWrite(params: InclusiveCacheParameters) extends InclusiveCacheBundle(params)
{
  val set  = UInt(width = params.setBits)
  val way  = UInt(width = params.wayBits)
  val data = new DirectoryEntry(params)
  def dump() = {
    DebugPrint(params, "DirectoryWrite: set: %x way: %x data: \n",
      set, way)
    data.dump()
  }
}

class DirectoryRead(params: InclusiveCacheParameters) extends InclusiveCacheBundle(params)
{
  val set = UInt(width = params.setBits)
  val tag = UInt(width = params.tagBits)
  def dump() = {
    DebugPrint(params, "DirectoryRead: set: %x tag: %x\n",
      set, tag)
  }
}

class DirectoryResult(params: InclusiveCacheParameters) extends DirectoryEntry(params)
{
  val hit = Bool()
  val way = UInt(width = params.wayBits)
  override def dump() = {
    DebugPrint(params, "DirectoryResult: dirty: %b state: %d clients: %x tag: %x hit: %b way: %x\n",
      dirty, state, clients, tag, hit, way)
  }
}

class Directory(params: InclusiveCacheParameters) extends Module
{
  val io = new Bundle {
    val write  = Decoupled(new DirectoryWrite(params)).flip
    val read   = Valid(new DirectoryRead(params)).flip // sees same-cycle write
    val result = Valid(new DirectoryResult(params))
    val ready  = Bool() // reset complete; can enable access
  }

  // dump
  when (io.write.fire()) {
    io.write.bits.dump()
  }

  when (io.read.fire()) {
    io.read.bits.dump()
  }

  when (io.result.fire()) {
    io.result.bits.dump()
  }

  /*
  when (io.ready) {
    DebugPrint(params, "Directory Ready\n")
  }
  */

  val codeBits = new DirectoryEntry(params).getWidth

  val singlePort = true
  val cc_dir = Module(new SRAMTemplate(UInt(width = codeBits), set=params.cache.sets, way=params.cache.ways,
    shouldReset=false, holdRead=false, singlePort=singlePort))

  val write = Queue(io.write, 1) // must inspect contents => max size 1
  // a flow Q creates a WaR hazard... this MIGHT not cause a problem
  // a pipe Q causes combinational loop through the scheduler

  // Wiping the Directory with 0s on reset has ultimate priority
  val wipeCount = RegInit(UInt(0, width = params.setBits + 1))
  val wipeOff = RegNext(Bool(false), Bool(true)) // don't wipe tags during reset
  val wipeDone = wipeCount(params.setBits)
  val wipeSet = wipeCount(params.setBits - 1,0)

  io.ready := wipeDone
  when (!wipeDone && !wipeOff) { wipeCount := wipeCount + UInt(1) }
  assert (wipeDone || !io.read.valid)

  // Be explicit for dumb 1-port inference
  val ren = io.read.valid
  val wen = (!wipeDone && !wipeOff) || write.valid
  assert (!io.read.valid || wipeDone)

  require (codeBits <= 256)

  // 我们使用的是单口的SRAM
  // 不能同时读和写
  // 这里是让读优先的
  write.ready := !io.read.valid
  cc_dir.io.w.req.valid := !ren && wen
  cc_dir.io.w.req.bits.apply(
    setIdx=Mux(wipeDone, write.bits.set, wipeSet),
    data=Mux(wipeDone, write.bits.data.asUInt, UInt(0)),
    waymask=UIntToOH(write.bits.way, params.cache.ways) | Fill(params.cache.ways, !wipeDone))

  val ren1 = RegInit(Bool(false))
  val ren2 = if (params.micro.dirReg) RegInit(Bool(false)) else ren1
  ren2 := ren1
  ren1 := ren

  val bypass_valid = params.dirReg(write.valid)
  val bypass = params.dirReg(write.bits, ren1 && write.valid)

  cc_dir.io.r.req.valid := ren
  cc_dir.io.r.req.bits.apply(setIdx=io.read.bits.set)
  val regout = params.dirReg(cc_dir.io.r.resp.data, ren1)

  val tag = params.dirReg(RegEnable(io.read.bits.tag, ren), ren1)
  val set = params.dirReg(RegEnable(io.read.bits.set, ren), ren1)

  // Compute the victim way in case of an evicition
  val replacer_array = Array.fill(params.cache.sets){
      ReplacementPolicy.fromString(params.cache.replacement, params.cache.ways)
  }
  val victimWay = Vec(replacer_array.map(_.way))(set)

  val setQuash = bypass_valid && bypass.set === set
  val tagMatch = bypass.data.tag === tag
  val wayMatch = bypass.way === victimWay

  val ways = Vec(regout.map(d => new DirectoryEntry(params).fromBits(d)))
  // 这边作为LLC，没有块儿权限之说，这里hit，不用检查权限
  val hits = Cat(ways.zipWithIndex.map { case (w, i) =>
    w.tag === tag && w.state =/= INVALID && (!setQuash || UInt(i) =/= bypass.way)
  }.reverse)
  val hit = hits.orR
  val hitWay = OHToUInt(hits)

  val wen1 = RegNext(write.fire(), init = false.B)
  val wen2 = if(params.micro.dirReg) RegNext(wen1, false.B) else wen1
  val writeWay1 = RegNext(write.bits.way)
  val writeWay2 = params.dirReg(writeWay1)
  val writeSet1 = RegNext(write.bits.set)
  val writeSet2 = params.dirReg(writeSet1)

  for((repl, i) <- replacer_array.zipWithIndex){
    val setMatch = i.U === Mux(ren2, set, writeSet2)
    assert(!(ren2 && wen2))
    val updateWay = Wire(UInt(params.wayBits.W))
    updateWay := Mux(ren2, hitWay, writeWay2)
    when(setMatch && ((ren2 && hit) || wen2)){
      repl.access(updateWay)
    }
  }

  val write_deq_forward = write.valid && write.bits.set === set && write.bits.data.tag === tag

  io.result.valid := ren2
  io.result.bits := Mux(hit,
    Mux(setQuash && tagMatch,
      bypass.data,
      Mux(write_deq_forward,
        write.bits.data,
        Mux1H(hits, ways))),
    //Mux1H(hits, ways),
    Mux(setQuash && (tagMatch || wayMatch), bypass.data, ways(victimWay)))
  io.result.bits.hit := hit || (setQuash && tagMatch && bypass.data.state =/= INVALID)
  io.result.bits.way := Mux(hit, OHToUInt(hits), Mux(setQuash && tagMatch, bypass.way, victimWay))

  params.ccover(ren2 && setQuash && tagMatch, "DIRECTORY_HIT_BYPASS", "Bypassing write to a directory hit")
  params.ccover(ren2 && setQuash && !tagMatch && wayMatch, "DIRECTORY_EVICT_BYPASS", "Bypassing a write to a directory eviction")

  def json: String = s"""{"clients":${params.clientBits},"mem":"cc_dir","clean":"${wipeDone.pathName}"}"""
}
