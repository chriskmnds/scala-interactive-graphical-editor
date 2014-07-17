
/*
 * Created by Christos Koumenides
 * 13 December 2012
 *
 * Scala 2.9.2
 *
 */

import scala.Console
import Array._

object ImageProc {

	def main(args: Array[String]): Unit = {
		
		readInput(Console.readLine(">"))
		
		//A simple recursive (tail) function to process user input until "X"
		//Not dealing with false input (e.g. letters instead of numbers)
		def readInput(input: String): Option[String] = {
			val x = input.trim.split(" ")
			x match { 
				case Array("I",_,_) =>
					Img.create(m = x(1).toInt, n = x(2).toInt)  match {
						case None =>  println("check your input")
						case _ => println("matrix created")
					}
				case Array("L",_,_,_) => 
					Img.colourPixel(x = x(1).toInt - 1, y = x(2).toInt - 1, c = x(3)) match {
						case None => println("check your input")
						case _ => println("pixel coloured")
					}
				case Array("V",_,_,_,_) => 
					Img.drawV(x = x(1).toInt - 1, y1 = x(2).toInt - 1, y2 = x(3).toInt - 1, c = x(4)) match {
						case None => println("check your input")
						case _ => println("pixels coloured")
					}
				case Array("H",_,_,_,_) => 
					Img.drawH(x1 = x(1).toInt - 1, x2 = x(2).toInt - 1, y = x(3).toInt - 1, c = x(4)) match {
						case None => println("check your input")
						case _ => println("pixels coloured")
					}
				case Array("F",_,_,_) => Img.fillR(x = x(1).toInt - 1, y = x(2).toInt - 1, c = x(3)) match {
						case None => println("check your input")
						case _ => println("pixels coloured")
					}
				case Array("C") => Img.reset; println("matrix reset")
				case Array("S") => println("=>"); Img.show
				case Array("X") => println("session terminated"); return None
				case _ => println("check your input")
			}
			readInput(Console.readLine(">"))
		}
	}
	
	//An Image object with a single variable "matrix" and a set of methods
	object Img {
			
		var matrix = Array[Array[String]]()
		
		//Create a new M x N image with all pixels coloured white (O)
		//M = columns (array elements)
		//N = rows (arrays)
		def create(m: Int, n: Int): Option[Any] = {
			if (m >= 1 && n >= 1 && n <= 250) {
				matrix = { for (i <- (1 to n)) yield Array.fill(m)("O") }.toArray
				Option(Some)
			}
			else None
		}
		
		//Clears the table, setting all pixels to white (O).
		def reset { 
			for (i <- 0 to matrix.length - 1; j <- 0 to matrix(i).length - 1) matrix(i)(j) = "O"
		}

		//Show the contents of the current image
		def show {
			matrix.foreach(x => { x.foreach(print); println })
		}

		//Colours the pixel at coordinate (X,Y) with colour C
		//try...catch to keep it working
		def colourPixel(x: Int, y: Int, c: String): Option[Any] = {
			try {
				matrix(y)(x) = c; Option(Some)
			} catch {
				case e: Exception => println(e.printStackTrace); None
			}
		}
		
		//Draw a vertical segment of colour C in column X between rows Y1 and Y2 (inclusive)
		//try...catch to keep it working
		def drawV (y1: Int, y2: Int, x: Int, c: String): Option[Any] = {
			try {
				for (i <- y1 to y2) matrix(i)(x) = c; Option(Some)
			} catch {
				case e: Exception => println(e.printStackTrace); None
			} 
		}
		
		//Draw a horizontal segment of colour C in row Y between columns X1 and X2 (inclusive)
		//try...catch to keep it working
		def drawH (x1: Int, x2: Int, y: Int, c: String): Option[Any] = {
			try {
				for (i <- x1 to x2) matrix(y)(i) = c; Option(Some)
			} catch {
				case e: Exception => println(e.printStackTrace); None
			} 
		}
		
		//Fill the region R with the colour C. R is defined as: Pixel (X,Y) belongs to R. 
		//Any other pixel which is the same colour as (X,Y) and shares a common side with 
		//any pixel in R also belongs to this region.
		//Method has a single variable "buff" to accumulate the results from the sub methods (not very functional)
		def fillR(x: Int, y: Int, c: String): Option[Any] = {
			try {
				//the current colour in R
				val pixel = matrix(y)(x)
				
				//buffer to accumulate results into
				var buff = List[(Int,Int)]()
				
				//traverse graph and render cells
				traverse(List((x,y))).foreach(x => matrix(x._2)(x._1) = c)
				
				
				//for each cell found to be in R we process its top, bottom, left and right cells 
				//to see if they are in R too. do this recursively until all paths from the initial 
				//cell are identified. Save the nodes in "buff" and return. Using "buff" this way is 
				//not ideal for a functional programming style, but my other option ended up being a 
				//head recursion, which I don't prefer.
				def traverse(list: List[(Int, Int)], visited: Array[(Int, Int)] = Array()): List[(Int,Int)] = {
					list match {
						case head :: tail if !visited.contains(head) => {
							traverse({ traverseBack(head) ::: traverseFront(head) ::: 
								traverseUp(head) ::: traverseDown(head) ::: tail}.distinct, visited ++ Array(head))
						}
						case head :: tail => traverse(tail, visited)
						case Nil => {(x,y) :: buff}.distinct
					}
				}
				
				//helper method
				def traverseBack(coord: (Int,Int)): List[(Int,Int)] = {
					coord._1 - 1 match {
						case x if x >= 0 && matrix(coord._2)(x).equals(pixel) => {
							buff = (x, coord._2) :: buff
							List((x, coord._2))
						}
						case _ => Nil
					}
				}
				
				//helper method
				def traverseFront(coord: (Int,Int)): List[(Int,Int)] = {
					coord._1 + 1 match {
						case x if x < matrix(coord._2).length && matrix(coord._2)(x).equals(pixel) => {
								buff = (x, coord._2) :: buff
								List((x, coord._2))
							}
						case _ => Nil
					}
				}
				
				//helper method
				def traverseUp(coord: (Int,Int)): List[(Int,Int)] = {
					coord._2 + 1 match {
						case x if x < matrix.length && matrix(x)(coord._1).equals(pixel) => {
								buff = (coord._1, x) :: buff
								List((coord._1, x))
							}
						case _ => Nil
					}
				}
				
				//helper method
				def traverseDown(coord: (Int,Int)): List[(Int,Int)] = {
					coord._2 - 1 match {
						case x if x >= 0 && matrix(x)(coord._1).equals(pixel) => {
								buff = (coord._1, x) :: buff
								List((coord._1, x))
							}
						case _ => Nil
					}
				}
				
				Option(Some)
			
			} catch {
				case e: Exception => println(e.printStackTrace); None
			}
		}
	}
}



