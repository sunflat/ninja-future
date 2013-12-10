import org.scalatest.matchers.ShouldMatchers
import org.scalatest.FreeSpec
import Main._
import TryExercise._
import scala.util.Failure
import scala.util.Success
import scala.util.Try
import org.scalatest.matchers.Matcher
import org.scalatest.matchers.MatchResult

/**
 * @author iwasaki
 */
class TryExerciseSpec extends FreeSpec with ShouldMatchers {
  import CountingSpec._
  import MyMatchers._

  val conv = new ConversationTry(affinity, message)
  
  s"シンプルなgenerate" - {
    List(
      ("hara", "maeda_", Failure(new NoSuchElementException)),
      ("maeda_", "c0hama_", Failure(new NoSuchElementException)),
      ("c0hama", "maeda_", Success("maeda_「連番クラス？」\nc0hama「連番クラス！」")),
      ("c0hama", "hara", Success("hara「Vim？」\nc0hama「Vim！」")),
      ("hara", "c0hama", Success("c0hama「Dart？」\nhara「Dart！」"))
    ) map { case (u1, u2, expected) =>
        s"""conversation from $u1 to $u2""" in {
          conv.generate_simple(u1, u2) should equalToTry (expected)
        }
    }
  }

  s"複雑なgenerate" - {
    List(
      ("hara", "maeda_", Failure(new NoSuchElementException)),
      ("maeda_", "c0hama_", Failure(new NoSuchElementException)),
      ("c0hama", "maeda_", Failure(new NoSuchElementException)),
      ("c0hama", "hara", Success("hara「連番クラス？」\nc0hama「連番クラス！」")),
      ("hara", "c0hama", Success("c0hama「Dart？」\nhara「Dart！」"))
    ) map { case (u1, u2, expected) =>
        s"""conversation from $u1 to $u2""" in {
          conv.generate_complex(u1, u2) should equalToTry (expected)
        }
    }
  }
}