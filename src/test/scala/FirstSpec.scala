import com.example.{LandingData, MarsLander}
import org.scalatest.FlatSpec

/**
  * Created by Dima on 31.08.2016.
  */
class FirstSpec extends FlatSpec {
  "MarsLander" should "land on flat ground" in {
    val ml = new MarsLander
//    ml.plotRoute(Queue(LandingData(2500,2700)))
  }
}
