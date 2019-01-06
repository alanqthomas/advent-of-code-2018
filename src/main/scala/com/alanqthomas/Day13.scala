package com.alanqthomas

import scala.collection.mutable.ArrayBuffer

object Day13 extends App {
  sealed trait Direction
  case object UP extends Direction
  case object DOWN extends Direction
  case object LEFT extends Direction
  case object RIGHT extends Direction
  case object STRAIGHT extends Direction

  object Track {
    def trackWithCart(track: Track, cart: Cart): Track = {
      track match {
        case _: ForwardCurve  => ForwardCurve(Some(cart))
        case _: BackCurve     => BackCurve(Some(cart))
        case _: Horizontal    => Horizontal(Some(cart))
        case _: Vertical      => Vertical(Some(cart))
        case _: Intersection  => Intersection(Some(cart))
        case BlankTrack       => BlankTrack
      }
    }

    def trackWithoutCart(track: Track): Track = {
      track match {
        case _: ForwardCurve  => ForwardCurve(None)
        case _: BackCurve     => BackCurve(None)
        case _: Horizontal    => Horizontal(None)
        case _: Vertical      => Vertical(None)
        case _: Intersection  => Intersection(None)
        case BlankTrack       => BlankTrack
      }
    }
  }

  sealed trait Track {val cart: Option[Cart] }
  case class Horizontal(cart: Option[Cart]) extends Track
  case class Vertical(cart: Option[Cart]) extends Track
  case class ForwardCurve(cart: Option[Cart]) extends Track
  case class BackCurve(cart: Option[Cart]) extends Track
  case class Intersection(cart: Option[Cart]) extends Track
  case object BlankTrack extends Track { val cart: Option[Cart] = None }

  case class CartCrash(otherCart: Cart)

  type TrackSystem = Array[Array[Track]]
  type Carts = Map[Int, Cart]

  case class Point(x: Int, y: Int) extends Ordered[Point] {
    def move(direction: Direction): Point = {
      direction match {
        case UP       => Point(x,     y - 1 )
        case DOWN     => Point(x,     y + 1 )
        case LEFT     => Point(x - 1, y     )
        case RIGHT    => Point(x + 1, y     )
        case STRAIGHT => throw new Exception("Invalid direction STRAIGHT")
      }
    }

    override def compare(that: Point): Int = {
      if ( y == that.y && x == that.x) 0
      else if (y == that.y) x.compareTo(that.x)
      else y.compareTo(that.y)
    }

    override def toString: String = s"($x, $y)"
  }

  case class Cart(location: Point, direction: Direction, turnDirection: Direction) extends Ordered[Cart] {
    def move(system: TrackSystem): Either[CartCrash, Cart] = {
      val newLocation @ Point(x, y) = location.move(direction)
      val nextTrack = system(y)(x)

      if (nextTrack.cart.nonEmpty) Left(CartCrash(nextTrack.cart.get))
      else {
        val (newDirection, newTurnDirection) = (direction, nextTrack) match {
          case (UP,     _: ForwardCurve)              => (RIGHT, turnDirection)
          case (UP,     _: BackCurve)                 => (LEFT, turnDirection)
          case (DOWN,   _: ForwardCurve)              => (LEFT, turnDirection)
          case (DOWN,   _: BackCurve)                 => (RIGHT, turnDirection)
          case (LEFT,   _: ForwardCurve)              => (DOWN, turnDirection)
          case (LEFT,   _: BackCurve)                 => (UP, turnDirection)
          case (RIGHT,  _: ForwardCurve)              => (UP, turnDirection)
          case (RIGHT,  _: BackCurve)                 => (DOWN, turnDirection)
          case (dir,    _: Horizontal | _: Vertical)  => (dir, turnDirection)
          case (STRAIGHT, _)                          => throw new Exception(s"Invalid direction STRAIGHT")
          case (_, BlankTrack)                        => throw new Exception("Tried to navigate onto a blank track")
          case (dir,    _: Intersection)              =>
            val newDir = Cart.turn(dir, turnDirection)
            val newTurnDir = Cart.getNewTurnDirection(turnDirection)
            (newDir, newTurnDir)
        }

        Right(Cart(newLocation, newDirection, newTurnDirection))
      }
    }

    override def compare(that: Cart): Int = this.location.compare(that.location)
    override def toString: String = s"${directionToChar(direction)} -> $location | $turnDirection"
  }

  object Cart {
    def turn(direction: Direction, turnDirection: Direction): Direction = {
      (turnDirection, direction) match {
        case (STRAIGHT, dir) => dir
        case (LEFT,  UP)    => LEFT
        case (LEFT,  DOWN)  => RIGHT
        case (LEFT,  LEFT)  => DOWN
        case (LEFT,  RIGHT) => UP
        case (RIGHT, UP)    => RIGHT
        case (RIGHT, DOWN)  => LEFT
        case (RIGHT, LEFT)  => UP
        case (RIGHT, RIGHT) => DOWN
        case _              =>
          throw new Exception(s"Invalid turn. Cart Direction: $direction; Turn Direction: $turnDirection")
      }
    }

    def getNewTurnDirection(direction: Direction): Direction = direction match {
      case LEFT     => STRAIGHT
      case STRAIGHT => RIGHT
      case RIGHT    => LEFT
      case _        => throw new Exception("Invalid turn direction")
    }
  }

  def updateSystem(system: TrackSystem, prevLocation: Point, cart: Cart): Unit = {
    val Point(prevX, prevY) = prevLocation
    val Point(nextX, nextY) = cart.location

    val prevTrack = system(prevY)(prevX)
    val nextTrack = system(nextY)(nextX)

    val newPrevTrack = Track.trackWithoutCart(prevTrack)
    val newNextTrack = Track.trackWithCart(nextTrack, cart)

    system(prevY)(prevX) = newPrevTrack
    system(nextY)(nextX) = newNextTrack
  }

  def charToTrack(c: Char, location: Point): Track = c match {
    case '-'  => Horizontal(None)
    case '|'  => Vertical(None)
    case '+'  => Intersection(None)
    case '/'  => ForwardCurve(None)
    case '\\' => BackCurve(None)
    case '^'  => Vertical(Some(Cart(location, UP, LEFT)))
    case 'v'  => Vertical(Some(Cart(location, DOWN, LEFT)))
    case '<'  => Horizontal(Some(Cart(location, LEFT, LEFT)))
    case '>'  => Horizontal(Some(Cart(location, RIGHT, LEFT)))
    case ' '  => BlankTrack
  }

  def directionToChar(direction: Direction): Char = direction match {
    case UP       => '^'
    case DOWN     => 'v'
    case LEFT     => '<'
    case RIGHT    => '>'
    case STRAIGHT => throw new Exception("No char for STRAIGHT")
  }

  def trackToChar(track: Track): Char = track match {
    case Horizontal(Some(cart))   => directionToChar(cart.direction)
    case Vertical(Some(cart))     => directionToChar(cart.direction)
    case Intersection(Some(cart)) => directionToChar(cart.direction)
    case ForwardCurve(Some(cart)) => directionToChar(cart.direction)
    case BackCurve(Some(cart))    => directionToChar(cart.direction)
    case Horizontal(None)         => '-'
    case Vertical(None)           => '|'
    case Intersection(None)       => '+'
    case ForwardCurve(None)       => '/'
    case BackCurve(None)          => '\\'
    case BlankTrack               => '.'
  }

  def printSystem(system: TrackSystem): Unit = {
    system.foreach(xs => println(xs.map(trackToChar).mkString))
    println()
  }

  // START
  val file = Common.getLinesFromFile("day13-input.txt")
  val systemWidth = file.head.length
  val systemHeight = file.length

  val system: TrackSystem = Array.ofDim[Track](systemHeight, systemWidth)
  val carts: ArrayBuffer[Cart] = ArrayBuffer()

  file.zipWithIndex.foreach {
    case (line, y) =>
      line.zipWithIndex.foreach {
        case (trackChar, x) =>
          val point = Point(x, y)
          val track = charToTrack(trackChar, point)
          system(y)(x) = track

          track.cart.map(c => carts += c)
      }
  }

  // Part 1
  def moveCarts(carts: Map[Int, Cart], system: TrackSystem): Either[CartCrash, Carts] = {
    def rec(cartsLeft: Vector[(Int, Cart)], newCarts: Carts): Either[CartCrash, Carts] = {
      if (cartsLeft.isEmpty) Right(newCarts)
      else {
        val (cartId, cart) = cartsLeft.head
        cart.move(system) match {
          case Left(crash) => Left(crash)
          case Right(newCart) =>
            updateSystem(system, cart.location, newCart)
            rec(cartsLeft.tail, newCarts + (cartId -> newCart))
        }
      }
    }

    val sortedCarts = carts.toVector.sortBy(_._2)
    rec(sortedCarts, Map.empty)
  }

  def findFirstCrash(system: TrackSystem, initialCarts: ArrayBuffer[Cart]): Point = {
    def rec(carts: Carts): Point = {
      moveCarts(carts, system) match {
        case Left(crash) => crash.otherCart.location
        case Right(newCarts) => rec(newCarts)
      }
    }

    val carts = initialCarts.zipWithIndex.map(ci => ci._2 -> ci._1).toMap
    rec(carts)
  }

  // Answer
//  val crashLocation = findFirstCrash(system, carts)
//  println(s"Crash Location = $crashLocation")


  // Part 2
  def cleanUpCarts(a: Cart, b: Cart, system: TrackSystem): Unit = {
    val Point(aX, aY) = a.location
    val Point(bX, bY) = b.location

    val trackA = system(aY)(aX)
    val trackB = system(bY)(bX)

    val newTrackA = Track.trackWithoutCart(trackA)
    val newTrackB = Track.trackWithoutCart(trackB)

    system(aY)(aX) = newTrackA
    system(bY)(bX) = newTrackB
  }

  def moveCartsAndRemoveCrashes(carts: Map[Int, Cart], system: TrackSystem): Carts = {
    def rec(cartsLeft: Vector[(Int, Cart)], newCarts: Carts): Carts = {
      if (cartsLeft.isEmpty) newCarts
      else {
        val (cartId, cart) = cartsLeft.head
        cart.move(system) match {
          case Left(crash) =>
            cleanUpCarts(cart, crash.otherCart, system)
            rec(
              cartsLeft.tail.filterNot(ic => ic._2.location == crash.otherCart.location),
              newCarts.filterNot(ic => ic._2.location == crash.otherCart.location)
            )
          case Right(newCart) =>
            updateSystem(system, cart.location, newCart)
            rec(cartsLeft.tail, newCarts + (cartId -> newCart))
        }
      }
    }

    val sortedCarts = carts.toVector.sortBy(_._2)
    rec(sortedCarts, Map.empty)
  }

  def findLastCartLocation(system: TrackSystem, initialCarts: ArrayBuffer[Cart]): Point = {
    def rec(carts: Carts): Point = {
      if (carts.size == 1) carts.head._2.location
      else rec(moveCartsAndRemoveCrashes(carts, system))
    }

    val carts = initialCarts.zipWithIndex.map(ci => ci._2 -> ci._1).toMap
    rec(carts)
  }

  // Answer
  val lastCartLocation = findLastCartLocation(system, carts)
  println(s"Last cart location = $lastCartLocation")
}
