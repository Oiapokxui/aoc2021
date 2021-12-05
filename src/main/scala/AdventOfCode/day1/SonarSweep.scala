package AdventOfCode.day1

import java.io.{BufferedReader, File, FileReader}
import java.nio.file.Paths
import java.util.Scanner

object SonarSweep {
  def countIncreases(input: List[Int]): Int = {
    case class sonarTuple (previousMeasurement:Int, numberOfIncreases:Int)

    val hasIncreased: (sonarTuple, Int) => sonarTuple = (lastResult, actualDepth) =>
        if actualDepth - lastResult.previousMeasurement > 0
        then sonarTuple(actualDepth, lastResult.numberOfIncreases + 1)
        else sonarTuple(actualDepth, lastResult.numberOfIncreases)

    val result = input.tail.foldLeft (sonarTuple(input.head, 0)) (hasIncreased)
    result.numberOfIncreases
  }

  def countIncreasesOfWindows(input: List[Int]): Int = {
    countIncreases(getThreeMeasurementsWindows(input, List()))
  }
 
  def getThreeMeasurementsWindows(input:List[Int], windows:List[List[Int]]): List[Int] = {
    if input.size >= 3
    then 
      val head::tail = input
      getThreeMeasurementsWindows(tail, input.take(3)::windows)
    else 
      windows.reverse.map(lista => lista.reduce(_+_))
  }

  def run(): Unit = {
    println(File("./").getCanonicalPath())
    val inputFile = Scanner(File("./inputs/sonarSweepInput.txt"))
    var input:List[Int] = List()
    while (inputFile.hasNext())  input = inputFile.nextInt() :: input
    input = input.reverse
    println("Day 1 - Sonar Sweep First Half: " + countIncreases(input))
    println("Day 1 - Sonar Sweep Second Half: " + countIncreasesOfWindows(input))
  }
}
