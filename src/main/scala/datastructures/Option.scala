package datastructures

/**
  * Option
  * --
  * Desc:
  * Changes:
  * 2016/4/28 - Created by z_jiang.
  * --
  */
sealed trait Option[+A] {
  def map[B](f: A => B): Option[B] = this match {
    case Some(get) => Some(f(get))
    case None => None
  }
  def flatMap[B](f: A => Option[B]): Option[B] = this match {
    case Some(get) => f(get)
    case None => None
  }
  def getOrElse[B >: A](default: => B): B = this match {
    case Some(get) => get
    case None => default
  }
  def orElse[B >: A](ob: => Option[B]): Option[B] = this match {
    case obj @ Some(get) => obj
    case None => ob
  }
  def filter(f: A => Boolean): Option[A] = this match {
    case obj @ Some(get) => if (f(get)) obj else None
    case None => None
  }

  def isEmpty: Boolean
}
case class Some[+A](get: A) extends Option[A] {
  override def isEmpty = false
}
case object None extends Option[Nothing] {
  override def isEmpty = true
}
