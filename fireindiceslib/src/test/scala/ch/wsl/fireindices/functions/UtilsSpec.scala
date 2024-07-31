package ch.wsl.fireindices.functions

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.time.LocalDate

class UtilsSpec extends AnyFlatSpec with Matchers {

  "Dates" should "converted to long and and LocalDate correctly" in {
    for{
      year <- 1980 to 2040
      month <- 1 to 12
      day <- 1 to 28
    } yield {
      Utils.longDate2LocalDate(Utils.solarDate2Long(s"$year${"%02d".format(month)}${"%02d".format(day)}")) shouldBe LocalDate.of(year,month,day)
    }

  }

}
