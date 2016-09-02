import com.example.{LandingData, MarsLander}
import org.scalatest.FlatSpec

/**
  * Created by Dima on 31.08.2016.
  */
class FirstSpec extends FlatSpec {
  "MarsLander" should "land on flat ground traversing easy on the right" in {
    

    val ml = new MarsLander(List((0, 100), (1000, 500), (1500, 1500), (3000, 1000), (4000, 150), (5500, 150), (6999, 800)))
    val e = ml.plotRoute(List(LandingData(2500,2700))).last
    assert(e.y <= ml.alt)
    assert(e.x >= ml.p1 && e.x <= ml.p2)
    assert(e.hs >= -20 && e.hs <= 20)
    assert(e.vs >= -40 && e.vs <= 40)
  }
}
