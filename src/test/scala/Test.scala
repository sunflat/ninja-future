import org.scalatest.matchers.ShouldMatchers
import org.scalatest.FreeSpec

import Main._

class CountingSpec extends FreeSpec with ShouldMatchers{ 
  val affinity: Map[User ==> User, Emotion] = Map(
    ("c0hama" ==> "hara", Love),
    ("hara" ==> "c0hama", Love),
    ("c0hama" ==> "maeda_", Hate)
  )

  val message:Map[User ==> Emotion, String] = Map(
    ("hara" ==> Love, "Dart"),
    ("c0hama" ==> Love, "Vim"),
    ("c0hama" ==> Hate, "連番クラス")
  )

  val conv = new Conversation(affinity, message)

  s"シンプルなgenerate" - {
    List(
      ("hara", "maeda_", None),
      ("maeda_", "c0hama_", None),
      ("c0hama", "maeda_", Some("maeda_「連番クラス？」\nc0hama「連番クラス！」")),
      ("c0hama", "hara", Some("hara「Vim？」\nc0hama「Vim！」")),
      ("hara", "c0hama", Some("c0hama「Dart？」\nhara「Dart！」"))
    ) map { case (u1, u2, expected) =>
        s"""conversation from $u1 to $u2""" in {
          conv.generate_simple(u1, u2) should equal (expected)
        }
    }
  }

  s"複雑なgenerate" - {
    List(
      ("hara", "maeda_", None),
      ("maeda_", "c0hama_", None),
      ("c0hama", "maeda_", None),
      ("c0hama", "hara", Some("hara「連番クラス？」\nc0hama「連番クラス！」")),
      ("hara", "c0hama", Some("c0hama「Dart？」\nhara「Dart！」"))
    ) map { case (u1, u2, expected) =>
        s"""conversation from $u1 to $u2""" in {
          conv.generate_complex(u1, u2) should equal (expected)
        }
    }
  }
}
