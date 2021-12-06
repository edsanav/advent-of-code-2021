package exercises

import cats.effect.ExitCode
import cats.effect.std.Console
import cats.syntax.apply.catsSyntaxApply
import cats.{Applicative, ApplicativeError}

import scala.annotation.tailrec
import scala.collection.immutable
import scala.collection.immutable.BitSet


object day3 {

  def solve[F[_] : Console : Applicative](input: String): F[ExitCode] =
    val lines = input.split("\n").to(List)
    Console[F].println((part1(lines), part2(lines))) *> Applicative[F].pure(ExitCode.Success)



  def part1(lines:List[String]):Int =
    val bitMaps = parseBits(lines)
    val bitLength =  lines.headOption.fold(0)(_.toList.length)
    val mostCommonBitSet = ((bitLength - 1) to 0 by -1).map(bitPos => mostCommonInPosition(bitMaps, bitPos))
    val gamma = Integer.parseInt(mostCommonBitSet.mkString, 2)
    val epsilonString = (~gamma).toBinaryString.takeRight(bitLength)
    val epsilon = Integer.parseInt(epsilonString, 2)
    gamma * epsilon

  def part2(lines:List[String]):Int = {
    val bitMaps = parseBits(lines)
    val bitLength =  lines.headOption.fold(0)(_.toList.length)
    val o2Rating = findRating(bitMaps, bitLength, mostCommonInPosition).toBitMask(0)
    val co2Rating = findRating(bitMaps, bitLength, leastCommonInPosition).toBitMask(0)
    (o2Rating * co2Rating).toInt
  }

  def binaryStringtoBitSet(bs:String):BitSet = BitSet.fromBitMask(Array(Integer.parseInt(bs, 2)))

  def parseBits(lines:List[String]):List[BitSet] = lines.map(binaryStringtoBitSet)

  def mostCommonInPosition(xs:List[BitSet], bitPosition:Int):Int =
    if (xs.count(_.contains(bitPosition)) >= xs.length/2.toFloat) 1 else 0

  def leastCommonInPosition(xs:List[BitSet], bitPosition:Int):Int =
    if (xs.count(_.contains(bitPosition)) < xs.length/2.toFloat) 1 else 0

  def inPosition(bs:BitSet, bitPosition:Int):Int = if (bs.contains(bitPosition)) 1 else 0

  def findRating(allBS:List[BitSet], bitLength:Int, criteria:(List[BitSet], Int) => Int):BitSet ={
    def go(bitSets:List[BitSet], bitPosition:Int):BitSet = bitSets match {
      case bs::Nil => bs
      case xs => {
        val mostCommon = criteria(bitSets, bitPosition)
        val filtered = xs.filter(bs => inPosition(bs, bitPosition) == mostCommon)
        go(filtered, bitPosition-1)
      }
    }
    go(allBS, bitLength-1)
  }
  
}