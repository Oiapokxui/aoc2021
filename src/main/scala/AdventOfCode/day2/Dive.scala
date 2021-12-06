package AdventOfCode.day2

import java.io.{BufferedReader, File, FileReader}
import java.nio.file.Paths
import java.util.Scanner
import scala.io.Source

object Dive {
  type Direction = "up" | "down" | "forward" 

  case class Step(direction: Direction, units: Int)

  case class DiveVector(horizontalPos: Int, depth: Int, aim: Int) {
    def +(that: DiveVector) = 
      DiveVector(this.horizontalPos + that.horizontalPos,
        this.depth + that.depth + this.aim * that.horizontalPos,
        this.aim + that.aim
      )
    def multiplyComponents = this.horizontalPos * this.depth
  }

  def stepToVector(step: (String, Int)): DiveVector = {
    step match {
      case ("up", unit) => DiveVector(0, -unit, 0)
      case ("down", unit) => DiveVector(0, unit, 0)
      case ("forward", unit) => DiveVector(unit, 0, 0)
      case _ => DiveVector(0,0,0)
    }
  }

  def stepToVectorFix(step: (String, Int)): DiveVector = {
    step match {
      case ("up", unit) => DiveVector(0, 0, -unit)
      case ("down", unit) => DiveVector(0, 0, unit)
      case ("forward", unit) => DiveVector(unit, 0, 0)
      case _ => DiveVector(0,0,0)
    }
  }

  def getWrongResultingDiveVector(steps: List[(String, Int)]): DiveVector = {
    steps.map(stepToVector).reduce(_+_)
  }

  def getResultingDiveVector(steps: List[(String, Int)]): DiveVector = {
    steps.map(stepToVectorFix).reduce(_+_)
  } 

  def run(): Unit = {
    val inputFile = "./inputs/diveInput.txt"
    val input:List[(String, Int)] = Source.fromFile(inputFile)
      .getLines
      .toList
      .map(line => line.split(" "))
      .map(lineSplit => (lineSplit(0), Integer.parseInt(lineSplit(1))))

    println("Day 2 - Dive! First Half: " + getWrongResultingDiveVector(input).multiplyComponents)
    println("Day 2 - Dive! First Half: " + getResultingDiveVector(input).multiplyComponents)
  }
}
