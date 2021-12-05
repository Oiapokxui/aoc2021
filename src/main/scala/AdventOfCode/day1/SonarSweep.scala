package AdventOfCode.day1

import java.io.{BufferedReader, File, FileReader}
import java.nio.file.Paths
import java.util.Scanner

object SonarSweep {
  def countIncreases(input: List[Int]): Int = {
    case class sonarTuple (previousMeasurement:Int, numberOfIncreases:Int)

    val hasIncreased: (sonarTuple, Int) => sonarTuple = (lastResult, actualDepth) =>
        if actualDepth - lastResult.previousMeasurement >= 0
        then sonarTuple(actualDepth, lastResult.numberOfIncreases + 1)
        else sonarTuple(actualDepth, lastResult.numberOfIncreases)

    val result = input.tail.foldLeft (sonarTuple(input.head, 0)) (hasIncreased)
    result.numberOfIncreases
  }

  def countIncreasesOfWindows(input: List[Int]): Int = {
    1
  }
  def getThreeMeasurementsWindows(input:List[Int], windowsSum:List[Int]): List[Int] = {
    type iterations = 0 | 1 | 2 | 3
    val inputHead::inputTail = input

    lazy val getThreeMeasurementsSum : (List[Int], List[Int], Int) => Int =
      (input, thisList, elementsInWindowList) =>
        val emptyInput = input match {
          case inputHead::inputTail => false
          case List() => true
        }
        if elementsInWindowList == 3 | emptyInput
        then thisList.reduce(_+_)
        else
          val inputHead::inputTail = input
          getThreeMeasurementsSum(inputTail, inputHead::thisList, elementsInWindowList + 1)

    val thisWindowSum = getThreeMeasurementsSum(input, List(), 0)
    if inputTail.isEmpty
    then thisWindowSum::windowsSum
    else getThreeMeasurementsWindows(inputTail, thisWindowSum::windowsSum)
  }

  def run(): Unit = {
    println(File("./").getCanonicalPath())
    val inputFile = Scanner(File("./inputs/sonarSweepInput.txt"))
    var input:List[Int] = List()
    while (inputFile.hasNext())  input = inputFile.nextInt() :: input
    println("Day 1 - Sonar Sweep: " + countIncreases(input.reverse))
    getThreeMeasurementsWindows(List(1,2,3,3), List())
  }
}
