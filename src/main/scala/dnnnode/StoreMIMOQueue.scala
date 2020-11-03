package dnnnode

import chisel3.experimental.{DataMirror, requireIsChiselType}
import chisel3.util._
import chisel3.{Flipped, Module, UInt, _}

class StoreMIMOQueueIO[T <: Data](private val gen: T, val entries: Int, NumIns: Int, NumOuts: Int) extends Bundle
{
  /** I/O to enqueue data (client is producer, and Queue object is consumer), is [[Chisel.DecoupledIO]] flipped. */
//  val enq = Flipped(EnqIO(gen))
  val enq = Flipped(EnqIO(Vec(NumIns, gen)))
  /** I/O to dequeue data (client is consumer and Queue object is producer), is [[Chisel.DecoupledIO]]*/
//  val deq = Flipped(DeqIO(gen))
  val deq = Flipped(DeqIO(Vec(NumOuts, gen)))
  /** The current amount of data in the queue */
  val count = Output(UInt(log2Ceil(entries + 1).W))

  val last = Input(Bool())
}

/** A hardware module implementing a Queue
  * @param gen The type of data to queue
  * @param entries The max number of entries in the queue
  * @param pipe True if a single entry queue can run at full throughput (like a pipeline). The ''ready'' signals are
  * combinationally coupled.
  * @param flow True if the inputs can be consumed on the same cycle (the inputs "flow" through the queue immediately).
  * The ''valid'' signals are coupled.
  *
  * @example {{{
  * val q = Module(new Queue(UInt(), 16))
  * q.io.enq <> producer.io.out
  * consumer.io.in <> q.io.deq
  * }}}
  */

class StoreMIMOQueue[T <: Data](gen: T,
                       val entries: Int, NumIns: Int, NumOuts: Int,
                       pipe: Boolean = false,
                       flow: Boolean = false)
                      (implicit compileOptions: chisel3.CompileOptions)
  extends Module() {
  require(entries > -1, "Queue must have non-negative number of entries")
  require(entries != 0, "Use companion object Queue.apply for zero entries")
  val genType = if (compileOptions.declaredTypeMustBeUnbound) {
    requireIsChiselType(gen)
    gen
  } else {
    if (DataMirror.internal.isSynthesizable(gen)) {
      chiselTypeOf(gen)
    } else {
      gen
    }
  }

  val io = IO(new StoreMIMOQueueIO(genType, entries, NumIns, NumOuts))

  val ram = Mem(entries, genType)
  val enq_ptr = RegInit(0.U((log2Ceil(entries)+1).W)) //Counter(entries)
  val deq_ptr = RegInit(0.U((log2Ceil(entries)+1).W)) //Counter(entries)
  val maybe_full = RegInit(false.B)

  val bufCount = io.count
  val ptr_match = enq_ptr === deq_ptr
  val empty = ptr_match && !maybe_full
  val full = ptr_match && maybe_full
  val do_enq = WireDefault(io.enq.fire())
  val do_deq = WireDefault(io.deq.fire())

  val last = RegInit(init = false.B)
  when(io.last) {last := true.B}

  when (do_enq) {
    for (i <- 0 until NumIns) {
      ram((enq_ptr + i.U) % entries.U) := io.enq.bits(i)
    }
    enq_ptr := (enq_ptr + NumIns.U) % entries.U
  }

  when (do_deq) {
//    deq_ptr.value := deq_ptr.value + NumOuts.U
    deq_ptr := (deq_ptr + NumOuts.U) % entries.U
    when ((last || io.last) && bufCount <= NumOuts.U) {
      deq_ptr := 0.U
      enq_ptr := 0.U
      last := false.B
    }
  }

  when(last) {
    io.enq.ready := false.B
  }.otherwise {
    io.enq.ready := !full
  }

  when (do_enq =/= do_deq) {
    maybe_full := do_enq
  }

//  io.enq.ready := !full

  val ptr_diff = enq_ptr - deq_ptr


  when (bufCount > (NumOuts - 1).U | last) {
    io.deq.valid := true.B
  }.otherwise {
    io.deq.valid := false.B
  }
  for (i <- 0 until NumOuts) {
    io.deq.bits(i) := ram((deq_ptr + i.U) % entries.U)
//    io.deq.bits(i) := Mux(bufCount > (NumOuts - 1).U, ram((deq_ptr + i.U) % entries.U),
//      Mux(i.U >= bufCount, 0.U.asTypeOf(genType), ram((deq_ptr + i.U) % entries.U)))

//    io.deq.bits(i) := Mux(bufCount > (NumOuts - 1).U, ram((deq_ptr + i.U) % entries.U), 0.U.asTypeOf(genType))
  }

  if (flow) {
    when (io.enq.valid) { io.deq.valid := true.B }
    when (empty) {
      io.deq.bits := io.enq.bits
      do_deq := false.B
      when (io.deq.ready) { do_enq := false.B }
    }
  }

  if (pipe) {
    when (io.deq.ready) { io.enq.ready := true.B }
  }

  if (isPow2(entries)) {
    io.count := Mux(maybe_full && ptr_match, entries.U, 0.U) | ptr_diff
  } else {
    io.count := Mux(ptr_match,
      Mux(maybe_full,
        entries.asUInt, 0.U),
      Mux(deq_ptr > enq_ptr,
        entries.asUInt + ptr_diff, ptr_diff))
  }
}



