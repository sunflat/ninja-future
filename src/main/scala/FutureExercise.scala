/**
 * @author iwasaki
 */
import scala.concurrent._
import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.Try
import scala.util.Success
import scala.util.Failure

object FutureExercise {
  import Main._
  
  var waitScale = 1000
  
  implicit class MapForFuture[A, B](val map:Map[A,B]) extends AnyVal {
    def getFuture(key:A):Future[B] = future {
      val wait = scala.util.Random.nextInt(waitScale*4) + waitScale
      println("begin wait "+ wait + ", " + key)
      Thread.sleep(wait)
      println("end wait "+ wait + ", " + key)
      //
      map(key)
    }
  }
  
  implicit class FutureEx[T](val a:Future[T]) extends AnyVal {
    def orElse(b:Future[T]):Future[T] = {
      val promise = Promise[T]()
      a.onComplete(promise.tryComplete(_))
      b.onComplete(promise.tryComplete(_))
      promise.future
    }
  }
  
  class ConversationFuture(
    affinity: Map[(User, User), Emotion],
    message: Map[(User, Emotion), String]
  ) {
    def generate_simple(u1: User, u2: User): Future[String] = {
      for(emotion <- affinity.getFuture((u1,u2)); msg <- message.getFuture((u1,emotion))) yield template(u1,u2,msg)
    }

    def generate_complex(u1: User, u2: User): Future[String] = {
      for{emotion1 <- affinity.getFuture((u1, u2))
          emotion2 <- affinity.getFuture((u2, u1)) if (emotion1 == emotion2)
          msgStr <- message.getFuture((u1, inverse(emotion1))).orElse(message.getFuture((u1, emotion1)))
      } yield template(u1, u2, msgStr)
    }
  }
}