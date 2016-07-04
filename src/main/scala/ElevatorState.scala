import java.io.{File, IOException}

import org.apache.commons.io.FileUtils

import scala.annotation.tailrec
import scala.util.{Failure, Success, Try}

case class ElevatorState(name: Char, time: Int, level: Int)

case class Link(from: ElevatorState, to: ElevatorState, weight: Int)

case class Graph(nodes: List[ElevatorState]) {

  val links : List[Link] = {
    val byKey: Map[(Int, Char), ElevatorState] = nodes.map{ es=>
      (es.time,es.name) -> es
    }.toMap
    val byTime: Map[Int, List[ElevatorState]] = nodes.groupBy(_.time)

    val links = List.newBuilder[Link]

    nodes.foreach{from =>
      byKey.get((from.time+1,from.name)).foreach{to=>
        links += Link(from,to,0)
      }

      byTime(from.time).filter(es=>es.name != from.name && es.level == from.level).foreach { transfer =>
        byKey.get((transfer.time+1,transfer.name)).foreach { to=>{
          links += Link(from,to,1)
        }}
      }
    }

    links.result
  }

  // djikstra's is likely overly ambitious.  Since the graph is a tree, I could simply walk
  // all reachable states in the correct depth, however I was thinking that transfers could be given
  // a positive weight and staying is zero such that transfers are minimized in the chosen path.

  private def djikstra(start: ElevatorState, targetTime: Int): List[Path] = {
    import scala.collection.mutable
    val paths = mutable.Map[ElevatorState,Option[Path]]()
    val unvisited = mutable.Set[ElevatorState]()

    val linksByState: Map[ElevatorState, List[Link]] = links.filter(_.to.time <= targetTime).groupBy(_.from)

    nodes.filter(_.time <= targetTime).foreach {es=>
      paths(es) = None
      unvisited.add(es)
    }

    paths(start) = Some(Path(Nil))

    def dist(es: ElevatorState) = paths(es).map(_.weight).getOrElse(Int.MaxValue)

    while(unvisited.nonEmpty) {
      val min: ElevatorState = unvisited.minBy(dist)
      unvisited.remove(min)
      paths(min).foreach { curPath=>
        linksByState.get(min).getOrElse(Nil).foreach{link=>
          val newPath = Path(curPath.links :+ link)
          if(newPath.weight < dist(link.to)) {
            paths(link.to) = Some(newPath)
          }
        }
      }
    }

    paths.values.toList.flatten
  }

  def pathTo(name: Char, level: Int, time: Int) : Try[Path] = {
    Try {
      nodes.filter{es=> es.time == 1 && es.name == name} match { // find start elevator with given name
        case Nil => throw new Exception(s"Elevator $name not found at time 1")
        case List(startState) =>
          if(level == startState.level && time == 1) { // if final level matches start state and time == 1, path is empty
            Path(Nil)
          } else {
            djikstra(startState,time).find{p=>
              p.links.nonEmpty &&
              p.lastState.level == level &&
              p.lastState.time == time
            }.getOrElse(throw new Exception("no path found"))
          }
        case _ => throw new Exception(s"unexpected condition, multiple $name elevators found at time 1")
      }
    }
  }
}

case class Path(links: List[Link]) {
  val weight = links.map(_.weight).sum
  def lastState = links.last.to
  override def toString = links.map(_.to.name).mkString
}


object ElevatorFileReader {

  def assertEqualLength(list: List[String]) : Boolean = {
    @tailrec
    def go(list: List[String],length: Int) : Boolean = {
      list match {
        case Nil => true
        case List(a) => a.length == length
        case x :: xs => x.length == length && go(xs, length)
      }
    }

    list match {
      case Nil => true
      case x :: xs => go(xs,x.length)
    }
  }

  def read(fileName: String) : Try[Graph] = {
    Try {
      val file = new File(fileName)
      if(!file.exists()) throw new IOException(s"file '$fileName' does not exist")

      val stringStates = FileUtils.readFileToString(file)
        .trim  // remove starting and trailing whitespace
        .replaceAll("\r","") // remove windows line endings
        .split("\n\\s*\n").toList // split on empty lines
        .map(_.replaceAll("\\s*\n","\n").trim) // remove trailing whitespace on individual lines

      if(!assertEqualLength(stringStates)) throw new IOException("parsing states shows uneven lengths after whitespace removed")

      // can do more validation here, validate shafts are consistent between states, elevators stay in their shafts, there
      // exists only one of each elevator in each state, if an elevator is in one state it is in them all, etc.

      val states = stringStates.zipWithIndex.flatMap(x=>readState(x._1,x._2 + 1))

      Graph(states)
    }


  }

  def readState(stringState: String, time: Int) : List[ElevatorState] = {
    stringState.split("\n").reverse.toList.zipWithIndex.flatMap { x =>
      val (line,idx) = x
      line.toCharArray.flatMap {ch=>
        if(ch <= 'Z' && ch >= 'A') Some(ElevatorState(ch,time,idx + 1)) else None
      }
    }
  }

  def pathTo(graph: Graph, start: String, destination: String): Try[Path] = {
    Try {
      if(!start.matches("[A-Z]")) throw new Exception("starting elevator must be a single capital letter")
      if(!destination.matches("\\d+-\\d+")) throw new Exception("final destination must be formatted as <floor>-<time>")
      val destArgs = destination.split("-",2).map(_.toInt)

      graph.pathTo(start.charAt(0),destArgs(0),destArgs(1)) match {
        case Failure(e) => throw e
        case Success(p) => p
      }
    }
  }

  /**
    * Write a command line program. It should take exactly three args as specified:
    **
    * ```
    * sbt "run <elevator system filename> <starting elevator> <final destination>"
    * ```
    **
    * Where `<final destination>` is specified as `<floor>-<time>`, e.g. 3-2, indicating that the final destination is the 3rd floor at time t=2.
    *
    * @param args
    */
  def main(args: Array[String]) = {
    args match {
      case Array(fileName, start, destination) =>
        read(fileName) match {
          case Failure(e) => System.err.println(e.getMessage)
          case Success(graph) =>
            pathTo(graph,start,destination) match {
              case Failure(e) => e.printStackTrace()
              case Success(path) =>
                println(path)
            }
        }
      case _ => System.err.println("invalid input, expecting three arguments in the form of '<elevator system filename> <starting elevator> <final destination>'")
    }
  }
}