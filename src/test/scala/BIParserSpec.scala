import org.scalatest.FunSpec
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class BIParserSpec extends FunSpec {
  import BIParser._
  
  describe("A Parser") {
    describe("parsing text") {
      it("should find the text") {
        val in = "\"52969d00# 393703: u_house_big_01_v1_f.p3d\""
        
        assert(parseAll(quotedText,in).get === "52969d00# 393703: u_house_big_01_v1_f.p3d")
      }
    }
    
    describe("parsing number") {
      it("should return double") {
        val in = "11.0003565"
          
        assert(parseAll(number,in).get === 11.0003565)
      }
    }
  }
}