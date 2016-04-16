package salma.simulation

import com.parctechnologies.eclipse.{OutOfProcessEclipse, EmbeddedEclipse, EclipseEngineOptions}
import org.scalatest.FunSuite

/**
  * Created by ckroiss on 21.12.15.
  */
class EclipseTest extends FunSuite {
  test("Simple Eclipse") {

    val options = new EclipseEngineOptions()
    options.setUseQueues(false)
    val eclipse = new OutOfProcessEclipse(options)
    val result = eclipse.rpc("write(output, 'hello world'), flush(output)")
    println(result)
    eclipse.destroy()


  }

}
