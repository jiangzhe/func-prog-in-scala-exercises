package par.actor

import java.util.concurrent.atomic.AtomicReference
import java.util.concurrent.{Callable, CountDownLatch, ExecutorService}

import akka.actor.{Actor, ActorSystem, Props}

/**
  * Future
  * --
  * Desc:
  * Changes:
  * 2016/5/13 - Created by z_jiang.
  * --
  */
sealed trait Future[+A] {
  private[par] def apply(k: A => Unit): Unit
}

object Future {
  val system = ActorSystem("future-system")

  type Par[+A] = ExecutorService => Future[A]

  def run[A](es: ExecutorService)(p: Par[A]): A = {
    val ref = new AtomicReference[A]
    val latch = new CountDownLatch(1)

    p(es) { a => ref.set(a); latch.countDown() }

    latch.await()
    ref.get()
  }

  def unit[A](a: A): Par[A] = es => new Future[A] {
    def apply(cb: A => Unit): Unit = cb(a)
  }

  def fork[A](a: => Par[A]): Par[A] = es => new Future[A] {
    def apply(cb: A => Unit): Unit = eval(es)(a(es)(cb))
  }

  def eval(es: ExecutorService)(r: => Unit): Unit = es.submit(new Callable[Unit] { def call = r })

  def map2[A,B,C](a: Par[A], b: Par[B])(f: (A,B) => C): Par[C] = es => new Future[C] {
    def apply(cb: C => Unit): Unit = {
      val holder = new AssistResultHolder[A,B](None, None)
      val combiner = system.actorOf(props(es, holder, f, cb), "assist")
      a(es)(av => combiner ! Left(av))
      b(es)(bv => combiner ! Right(bv))
    }
  }

  /**
    * Akka assist actor implementation
    * @param es
    * @param holder
    * @param f
    * @param cb
    * @tparam A
    * @tparam B
    * @tparam C
    */
  private class AssistActor[A,B,C](es: ExecutorService, holder: AssistResultHolder[A,B], f: (A,B) => C, cb: C => Unit) extends Actor {
    def receive = {
      case Left(a: A) => holder.ob match {
        case None => holder.oa = Some(a)
        case Some(b) => eval(es)(cb(f(a,b)))
      }
      case Right(b: B) => holder.oa match {
        case None => holder.ob = Some(b)
        case Some(a) => eval(es)(cb(f(a,b)))
      }
    }
  }

  /**
    * result holder
    * @param oa
    * @param ob
    * @tparam A
    * @tparam B
    */
  private class AssistResultHolder[A,B](var oa: Option[A], var ob: Option[B])

  def props[A,B,C](es: ExecutorService, holder: AssistResultHolder[A,B], f: (A,B) => C, cb: C => Unit) =
    Props(classOf[AssistActor[A,B,C]], es, holder, f, cb)




}
