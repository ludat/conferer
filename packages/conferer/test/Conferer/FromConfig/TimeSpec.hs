{-# LANGUAGE TypeApplications #-}

module Conferer.FromConfig.TimeSpec (spec) where

import Test.Hspec
import Conferer.FromConfig.Extended
import Data.Time
import Data.Time.Calendar.OrdinalDate (fromOrdinalDate)
import Data.Time.Calendar.WeekDate
import Data.Maybe

spec :: Spec
spec = do
  fcontext "Basic fetching" $ do
    describe "fetching a Day from config" $ do
      let
        februay3th2021 = fromOrdinalDate 2021 3
      ensureEmptyConfigThrows @Day
      ensureUsingDefaultReturnsSameValue @Day februay3th2021
      ensureSingleConfigParsesTheRightThing @Day "2021-01-03" februay3th2021
      ensureSingleConfigParsesTheRightThing @Day "0000-01-03" $ fromOrdinalDate 0 3
      describe "with missing leading zeros" $ do
        ensureSingleConfigThrowsParserError @Day "2021-1-3"
        ensureSingleConfigThrowsParserError @Day "2021-1-03"
        ensureSingleConfigThrowsParserError @Day "2021-01-3"

    describe "fetching a DayOfWeek" $ do
      ensureEmptyConfigThrows @DayOfWeek
      ensureUsingDefaultReturnsSameValue @DayOfWeek Monday
      ensureSingleConfigParsesTheRightThing @DayOfWeek "Monday" Monday
      context "with a lower case day" $ do
        ensureSingleConfigParsesTheRightThing @DayOfWeek "monday" Monday
      context "with a three letter abreviation" $ do
        ensureSingleConfigParsesTheRightThing @DayOfWeek "mon" Monday

    describe "fetching a TimeOfDay from config" $ do
      ensureEmptyConfigThrows @TimeOfDay
      ensureUsingDefaultReturnsSameValue @TimeOfDay midnight
      ensureSingleConfigParsesTheRightThing @TimeOfDay "00:30:00" $ fromJust $ makeTimeOfDayValid 0 30 0
      ensureSingleConfigParsesTheRightThing @TimeOfDay "00:30:00.5" $ fromJust $ makeTimeOfDayValid 0 30 0.5
      ensureSingleConfigParsesTheRightThing @TimeOfDay "00:00:00" $ midnight

      describe "with values that overflow" $ do
        ensureSingleConfigThrowsParserError @TimeOfDay "70:00:00"
        ensureSingleConfigThrowsParserError @TimeOfDay "00:70:00"
        ensureSingleConfigThrowsParserError @TimeOfDay "00:00:70"
