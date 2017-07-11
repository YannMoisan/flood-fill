import scala.annotation.tailrec
import scala.collection.immutable.Queue

object FloodFill {

  def size(x: Int, y: Int, grid: Seq[String]): Int = {
    @tailrec
    def size0(q: Queue[Pos], visited: Set[Pos]): Int = {
      q.dequeueOption match {
        case None => visited.size
        case Some((p, q2)) =>
          if (visited.contains(p)) {
            size0(q2, visited)
          } else {
            if (grid(p.y)(p.x) == 'o') {
              val q3 = Pos.validNeighbors(p, grid).foldLeft(q2) {
                case (q, i) => q.enqueue(i)
              }
              size0(q3, visited + p)
            } else {
              size0(q2, visited)
            }
          }
      }
    }
    size0(Queue[Pos](Pos(x, y)), Set.empty)
  }
}

case class Pos(x: Int, y: Int)

object Pos {
  def neighbors(p: Pos) =
    Seq(Pos(p.x + 1, p.y),
        Pos(p.x - 1, p.y),
        Pos(p.x, p.y - 1),
        Pos(p.x, p.y + 1))
  def validNeighbors(p: Pos, grid: Seq[String]) = neighbors(p).filter {
    case Pos(x, y) =>
      x >= 0 && x < grid(0).length && y >= 0 && y < grid.length
  }
}
