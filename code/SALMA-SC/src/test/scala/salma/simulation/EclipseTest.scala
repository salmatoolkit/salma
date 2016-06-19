package salma.simulation

import java.io.File

import com.parctechnologies.eclipse.{EclipseEngineOptions, EmbeddedEclipse, OutOfProcessEclipse, Varia}
import org.scalatest.FunSuite

/**
  * Created by ckroiss on 21.12.15.
  */
class EclipseTest extends FunSuite {
  test("Simple Eclipse") {

    val options = new EclipseEngineOptions()
    options.setUseQueues(false)
    options.setEclipseDir(new File("/opt/eclipseclp"))
    val eclipse = new OutOfProcessEclipse(options)
    eclipse.rpc("X = 1")
    (1 to 10) foreach { i =>
      val result = eclipse.rpc("write(output, 'hello world'), flush(output)")
      println(result)
    }
    eclipse.destroy()


  }

}
