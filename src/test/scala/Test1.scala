import org.junit.Test
import org.junit.Assert.*

class Test1:
  def msg = "I was compiled by Scala 3. :)"

  @Test def t1(): Unit = 
    assertEquals("I was compiled by Scala 3. :)", msg)
