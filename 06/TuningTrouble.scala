#!/usr/bin/env scala

import scala.io.Source

object Day6 extends App {

  val packetMarkerLength  = 4
  val messageMarkerLength = 14

  def findMarkerIndex(signal: String, markerLength: Int): Option[Int] = {
    val slideIndex = signal
      .sliding(markerLength)
      .indexWhere(_.distinct.length == markerLength)

    Option(slideIndex).filter(_ >= 0).map(_ + markerLength)
  }


  val source = args.headOption.fold(Source.stdin)(Source.fromFile)

  val markers = source.getLines()
    .flatMap { signal =>
      for {
        startOfPacket  <- findMarkerIndex(signal, packetMarkerLength)
        startOfMessage <- findMarkerIndex(signal, messageMarkerLength)
      } yield (startOfPacket, startOfMessage)
    }
    .to(LazyList)

  println(s"Part 1: ${markers.map(_._1).mkString(",")}")
  println(s"Part 2: ${markers.map(_._2).mkString(",")}")

}
