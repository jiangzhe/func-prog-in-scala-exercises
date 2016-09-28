package applicative

import monad.{Mon, Monad, Functor}
import monoid.{Foldable, Monoid}
import state.State

/**
  * Traverse
  * Author: zhe.jiang
  * Desc:
  * Change log:
  * 2016/9/24 - created by zhe.jiang
  */
trait Traverse[F[_]] {

//  def traverse[G[_]: Applicative, A, B](fa: F[A])(f: A => G[B]): G[F[B]]

  def traverse[G[_], A, B](fa: F[A])(f: A => G[B])(implicit g: Applicative[G]): G[F[B]]

  def sequence[G[_]: Applicative, A](fga: F[G[A]]): G[F[A]] = traverse(fga)(ga => ga)

  def traverseS[S,A,B](fa: F[A])(f: A => State[S, B]): State[S, F[B]] =
    traverse[({type H[X] = State[S, X]})#H, A, B](fa)(f)(Monad.stateMonad)

//  def zipWithIndex[A](ta: F[A]): F[(A, Int)] =
//    traverseS(ta)(a =>
//      for {
//        i <- State.get[Int]
//        _ <- State.set(i + 1)
//      } yield (a, i)
//    ).run(0)._1

//  def toList[A](fa: F[A]): List[A] =
//    traverseS(fa)(a =>
//      for {
//        as <- State.get[List[A]]
//        _ <- State.set(a :: as)
//      } yield ()
//    ).run(Nil)._2.reverse

  def mapAccum[S,A,B](fa: F[A], s: S)(f: (A, S) => (B, S)): (F[B], S) =
    traverseS(fa)((a: A) =>
      for {
        s1 <- State.get[S]
        (b, s2) = f(a, s1)
        _ <- State.set(s2)
      } yield b
    ).run(s)

  def toList[A](fa: F[A]): List[A] =
    mapAccum(fa, List.empty[A])((a, s) => ((), a :: s))._2.reverse

  def zipWithIndex[A](fa: F[A]): F[(A, Int)] =
    mapAccum(fa, 0)((a, s) => ((a, s), s + 1))._1

  def reverse[A](fa: F[A]): F[A] = {
    val ls = mapAccum(fa, List.empty[A])((a, s) => ((), a :: s))._2
    mapAccum(fa, ls) {
      case (a, h :: t) => (h, t)
      case _ => throw new RuntimeException("invalid state")
    }._1
  }

  def foldLeft[A, B](fa: F[A])(z: B)(f: (B, A) => B): B =
    mapAccum(fa, z)((a, s) => (f(s, a), f(s, a)))._2

  def zip[A, B](fa: F[A], fb: F[B]): F[(A, B)] = mapAccum(fa, toList(fb)){
    case (a, Nil) => sys.error("zip: Incompatible shapes")
    case (a, b :: bs) => ((a, b), bs)
  }._1

  def zipL[A,B](fa: F[A], fb: F[B]): F[(A, Option[B])] =
    mapAccum(fa, toList(fb)){
      case (a, Nil) => ((a, None), Nil)
      case (a, b :: bs) => ((a, Some(b)), bs)
    }._1

  def zipR[A,B](fa: F[A], fb: F[B]): F[(Option[A], B)] =
    mapAccum(fb, toList(fa)){
      case (b, Nil) => ((None, b), Nil)
      case (b, a :: as) => ((Some(a), b), as)
    }._1

  import Applicative.ApplicativeExt

  def fuse[G[_], H[_], A, B](fa: F[A])(g: A => G[B], h: A => H[B])
                            (G: Applicative[G], H: Applicative[H]): (G[F[B]], H[F[B]]) = {
    implicit val GHA = G.productA(H)
    traverse[({type M[X] = (G[X], H[X])})#M, A, B](fa)(a =>
      (g(a), h(a))
    )(GHA)
  }

}

object Traverse {
  val listTraverse = new Traverse[List] {
    override def traverse[G[_] : Applicative, A, B](fa: List[A])(f: (A) => G[B]): G[List[B]] = {
      val G = implicitly[Applicative[G]]
      fa.foldRight(G.unit(List.empty[B]))((a, acc) => G.map2(f(a), acc)(_ :: _))
    }
  }

  val optionTraverse = new Traverse[Option] {
    override def traverse[G[_] : Applicative, A, B](fa: Option[A])(f: (A) => G[B]): G[Option[B]] = {
      val G = implicitly[Applicative[G]]
//      fa.foldRight(G.unit(Option.empty[B]))((a, acc) => G.map2(f(a), acc)((b, _) => Some(b)))
      fa match {
        case Some(a) => G.map2(f(a), G.unit(()))((b, _) => Some(b))
        case None => G.unit(None)
      }
    }
  }

  val treeTraverse = new Traverse[Tree] {
    override def traverse[G[_] : Applicative, A, B](fa: Tree[A])(f: (A) => G[B]): G[Tree[B]] = {
      val G = implicitly[Applicative[G]]
      G.map2(f(fa.head), listTraverse.sequence(fa.tail.map(tree => traverse(tree)(f))))((hd, tl) => Tree(hd, tl))
    }
  }

  type Id[A] = A
  implicit val idApplicative = new Applicative[Id] {
    override def map2[A, B, C](fa: Id[A], fb: Id[B])(f: (A, B) => C): Id[C] = f(fa, fb)

    override def unit[A](a: => A): Id[A] = a
  }

  implicit class TraverseExt[F[_]](val traverse: Traverse[F]) extends Functor[F] {
    override def map[A, B](fa: F[A])(f: (A) => B): F[B] =
      traverse.traverse[Id, A, B](fa)(a => f(a))
  }

  type Const[M, B] = M

  implicit def monoidApplicative[M](M: Monoid[M]) =
    new Applicative[({type F[X] = Const[M, X]})#F] {
      override def map2[A, B, C](fa: Const[M, A], fb: Const[M, B])(f: (A, B) => C): Const[M, C] =
        M.op(fa, fb)

      override def unit[A](a: => A): Const[M, A] = M.zero
    }

  implicit class TraverseExt2[F[_]](val traverse: Traverse[F]) extends Functor[F] with Foldable[F]
  {
    override def map[A, B](fa: F[A])(f: (A) => B): F[B] = traverse.traverse[Id, A, B](fa)(a => f(a))

    override def foldMap[A, B](as: F[A])(f: (A) => B)(mb: Monoid[B]): B =
      traverse.traverse[({type H[X] = Const[B, X]})#H, A, B](as)(f)(mb)

    override def foldLeft[A, B](as: F[A])(z: B)(f: (B, A) => B): B = {
      val g = (a: A) => (b: B) => f(b, a)
      val bs: F[B => B] = map(as)(g)
      val m = new Monoid[B => B] {
        override def op(a1: (B) => B, a2: (B) => B): (B) => B = b => a2(a1(b))

        override def zero: (B) => B = b => b
      }
      foldMap[B => B, B => B](bs)(b => b)(m)(z)
    }

    override def foldRight[A, B](as: F[A])(z: B)(f: (A, B) => B): B = {
      foldLeft(as)(z)((b, a) => f(a, b))
    }
  }

  implicit class TraverseExt3[F[_]](val F: Traverse[F]) {
    def compose[K[_]](implicit K: Traverse[K]): Traverse[({type H[X] = F[K[X]]})#H] =
      new Traverse[({type H[X] = F[K[X]]})#H] {
        override def traverse[G[_], A, B](fa: F[K[A]])(f: (A) => G[B])(implicit G: Applicative[G]): G[F[K[B]]] =
          F.traverse(fa)(ka =>
            K.traverse(ka)(a =>
              f(a)
            )
          )
      }
  }

  implicit class TraverseExt4[F[_]](val F: Traverse[F]) {
    def composeM[F[_], G[_]](F: Mon[F], G: Mon[G], T: Traverse[G]): Mon[({type H[X] = F[G[X]]})#H] =
      new Mon[({type H[X] = F[G[X]]})#H] {
        override def flatMap[A, B](fga: F[G[A]])(f: (A) => F[G[B]]): F[G[B]] = {
          implicit val FA: Applicative[F] = F
          implicit val GA: Applicative[G] = G
          F.flatMap(fga)(ga => // to fgb
            F.map(T.traverse(ga)(a => f(a)))(ggb => G.join(ggb))
          )
        }


        override def unit[A](a: => A): F[G[A]] = F.unit(G.unit(a))
      }
  }

}

case class Tree[+A](head: A, tail: List[Tree[A]])


object Run {
  import Traverse._
  def main(args: Array[String]): Unit = {
    val list = List(1,2,3,4)
    val result1 = listTraverse.foldLeft(list)(1)(_ * _)
    println(result1)
    val result2 = listTraverse.foldRight(list)(1)(_ * _)
    println(result2)
    val result3 = listTraverse.zipWithIndex(list)
    println(result3)

    println(
      (for {
        i1 <- State.get[Int]
        i2 <- State.set(i1 + 1)
      } yield i2).run(100)
    )

    val result4 = listTraverse.toList(list)
    println(result4)
    val result5 = listTraverse.reverse(list)
    println(result5)
    val result6 = listTraverse.foldLeft(list)(0)(_ + _)
    println(result6)
  }
}

case class OptionT[M[_], A](value: M[Option[A]])(implicit M: Mon[M]) {
  def flatMap[B](f: A => OptionT[M, B]): OptionT[M, B] =
    OptionT(M.flatMap(value){
      case None => M.unit(None)
      case Some(a) => f(a).value
    })
}