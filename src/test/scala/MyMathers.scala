/**
 * @author iwasaki
 */
import scala.util.Try
import scala.util.Failure
import scala.util.Success
import org.scalatest.matchers.Matcher
import org.scalatest.matchers.MatchResult
import scala.concurrent.Future
import scala.concurrent.Await
import scala.concurrent.duration._

object MyMatchers {
  def tryEq[A](v:Try[A],expected:Try[A]):Boolean = {
    expected match {
      case Success(_) => v == expected
      case Failure(ex1) => v match {
        case Failure(ex2) => ex1.getClass() == ex2.getClass()
        case _ => false
      }
    }
  }
  
  def equal2[T](eq:(T,T)=>Boolean)(right:T) =
    Matcher { (left: T) =>
      MatchResult(eq(left,right),
        left + " did not equal " + right,
        left + " equals" + right
      )
    }
  
  val equalToTry = equal2(tryEq)_
  
  def futureToTrySync[A](v:Future[A]):Try[A] = {
    Await.ready(v,Duration.Inf)
    v.value.get
  }
  
  /*
  def successMatcher[T] = Matcher { (v:T) => MatchResult(true,"successMatcher","successMatcher") }
  def failureMatcher[T] = Matcher { (v:T) => MatchResult(false,"failureMatcher","failureMatcher") }
  */
}

