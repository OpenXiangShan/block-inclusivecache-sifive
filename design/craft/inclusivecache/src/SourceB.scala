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

import chisel3._
import chisel3.util._
import freechips.rocketchip.tilelink._
import freechips.rocketchip.util._

class SourceBRequest(params: InclusiveCacheParameters) extends InclusiveCacheBundle(params)
{
  val param   = UInt(3.W)
  val tag     = UInt(params.tagBits.W)
  val set     = UInt(params.setBits.W)
  val clients = UInt(params.clientBits.W)
  def dump() = {
    DebugPrint(params, "SourceBRequest: param: %x tag: %x set: %x clients: %x addr %x\n",
      param, tag, set, clients, (tag << (params.setBits + params.offsetBits) | set << (params.offsetBits)))
  }
}

class SourceB(params: InclusiveCacheParameters) extends Module with HasTLDump
{
  val io = IO(new Bundle {
    val req = Flipped(Decoupled(new SourceBRequest(params)))
    val b = Decoupled(new TLBundleB(params.inner.bundle))
  })

  /*
  when (io.b.fire) {
    DebugPrint(params, "inner probe ")
    io.b.bits.dump
  }
  */

  when (io.req.fire) {
    DebugPrint(params, "sourceB req ")
    io.req.bits.dump
  }

  if (params.firstLevel) {
    // 如果是firstLevel，肯定就不需要发送probe了
    // Tie off unused ports
    io.req.ready := true.B
    io.b.valid := false.B
  } else {
    val remain = RegInit(0.U(params.clientBits.W))
    val remain_set = WireInit(0.U(params.clientBits.W))
    val remain_clr = WireInit(0.U(params.clientBits.W))
    remain := (remain | remain_set) & ~remain_clr

    val busy = remain.orR
    // 哪些是要处理的，如果有busy的话就用remain，否则就直接用clients
    val todo = Mux(busy, remain, io.req.bits.clients)
    // next是随机选中的，下一个要处理的？
    // 问题，为啥处处都是随机的呢？why？
    val next = ~(leftOR(todo) << 1) & todo

    if (params.clientBits > 1) {
      params.ccover(PopCount(remain) > 1.U, "SOURCEB_MULTI_PROBE", "Had to probe more than one client")
    }

    assert (!io.req.valid || io.req.bits.clients =/= 0.U)

    io.req.ready := !busy
    // 这里是标注一下，有哪些client需要被probe
    when (io.req.fire) { remain_set := io.req.bits.clients }

    // No restrictions on the type of buffer used here
    val b = Wire(chiselTypeOf(io.b))
    b := DontCare
    io.b <> params.micro.innerBuf.b(b)

    b.valid := busy || io.req.valid
    when (b.fire) { remain_clr := next }
    params.ccover(b.valid && !b.ready, "SOURCEB_STALL", "Backpressured when issuing a probe")

    val tag = Mux(!busy, io.req.bits.tag, RegEnable(io.req.bits.tag, io.req.fire))
    val set = Mux(!busy, io.req.bits.set, RegEnable(io.req.bits.set, io.req.fire))
    val param = Mux(!busy, io.req.bits.param, RegEnable(io.req.bits.param, io.req.fire))

    b.bits.opcode  := TLMessages.Probe
    b.bits.param   := param
    b.bits.size    := params.offsetBits.U
    b.bits.source  := params.clientSource(next)
    b.bits.address := params.expandAddress(tag, set, 0.U)
    b.bits.mask    := ~0.U(params.inner.manager.beatBytes.W)
    b.bits.data    := 0.U
  }
}
