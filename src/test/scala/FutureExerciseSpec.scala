/**
 * @author iwasaki
 */
import org.scalatest.matchers.ShouldMatchers
import org.scalatest.FreeSpec
import Main._
import FutureExercise._
import scala.util.Failure
import scala.util.Success
import scala.util.Try
import org.scalatest.matchers.Matcher
import org.scalatest.matchers.MatchResult
import org.scalatest.matchers.ShouldMatchers
import org.scalatest.BeforeAndAfter

class FutureExerciseSpec extends FreeSpec with ShouldMatchers with BeforeAndAfter {
  import CountingSpec._
  import MyMatchers._

  val conv = new ConversationFuture(affinity, message)
  
  s"シンプルなgenerate" - {
    List(
      ("hara", "maeda_", Failure(new NoSuchElementException)),
      ("maeda_", "c0hama_", Failure(new NoSuchElementException)),
      ("c0hama", "maeda_", Success("maeda_「連番クラス？」\nc0hama「連番クラス！」")),
      ("c0hama", "hara", Success("hara「Vim？」\nc0hama「Vim！」")),
      ("hara", "c0hama", Success("c0hama「Dart？」\nhara「Dart！」"))
    ) map { case (u1, u2, expected) =>
        s"""conversation from $u1 to $u2""" in {
          futureToTrySync(conv.generate_simple(u1, u2)) should equalToTry (expected)
        }
    }
  }

  s"複雑なgenerate" - {
    List(
      ("hara", "maeda_", List(Failure(new NoSuchElementException))),
      ("maeda_", "c0hama_", List(Failure(new NoSuchElementException))),
      ("c0hama", "maeda_", List(Failure(new NoSuchElementException))),
      ("c0hama", "hara", List(Success("hara「Vim？」\nc0hama「Vim！」"),Success("hara「連番クラス？」\nc0hama「連番クラス！」"))),
      ("hara", "c0hama", List(Success("c0hama「Dart？」\nhara「Dart！」"),Failure(new NoSuchElementException)))
    ) map { case (u1, u2, expectedList) =>
        s"""conversation from $u1 to $u2""" in {
          for(i <- 1 to 10) {
            futureToTrySync(conv.generate_complex(u1, u2)) should
              expectedList.map(equalToTry(_)).reduceRight(_ or _)
          }
        }
    }
  }
  
  var oldWaitScale:Int=0;
  before {
    oldWaitScale = FutureExercise.waitScale
    FutureExercise.waitScale = 10
  }
  after {
    FutureExercise.waitScale = oldWaitScale
  }
}