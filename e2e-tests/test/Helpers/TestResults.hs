{-# OPTIONS_GHC -Wno-missing-import-lists #-}
{-# OPTIONS_GHC -Wno-missing-fields #-}
{-# LANGUAGE RecordWildCards #-}
module Helpers.TestResults (
    TestInfo(..),
    TestSuiteResults(..),
    TestResult(..),
    testSuitesToJUnit
) where

import Data.Maybe (fromJust)
import Text.XML.Light (Attr (Attr), CData (CData), CDataKind (CDataText), Content (Elem, Text), Element (..),
                       QName (QName))

data TestInfo = TestInfo {
    testName        :: String,
    testDescription :: String
} deriving Show

data TestSuiteResults = TestSuiteResults {
    suiteName    :: String,
    suiteResults ::  [TestResult]
} deriving Show

data TestResult = TestResult {
    resultTestInfo   :: TestInfo,
    resultSuccessful :: Bool,
    resultFailure    :: Maybe String, -- TODO: use this in failureElement in xml output
    resultTime       :: Double
} deriving Show

defElement :: Element
defElement = Element {elLine = Nothing}

-- top level - all Property test suites
testSuitesToJUnit :: [TestSuiteResults] -> Element
testSuitesToJUnit testSuites = defElement
  { elName = QName "testsuites" Nothing Nothing
  , elAttribs = [ Attr (QName "name" Nothing Nothing) "Antaeus E2E Tests"
                , Attr (QName "tests" Nothing Nothing)
                    (show $ map (\testSuite -> length $ suiteResults testSuite) testSuites)
                ]
  , elContent = map (Elem . testSuiteToJUnit) testSuites
  }

-- each Property (e.g. pv6Tests)
testSuiteToJUnit :: TestSuiteResults -> Element
testSuiteToJUnit TestSuiteResults{..} = defElement
  { elName = QName "testsuite" Nothing Nothing
  , elAttribs = [ Attr (QName "name" Nothing Nothing) suiteName
                , Attr (QName "tests" Nothing Nothing) (show $ length suiteResults)
                ]
  , elContent = map (Elem . testCaseToJUnit suiteName) suiteResults
  }

-- each runTest within Property
testCaseToJUnit :: String -> TestResult -> Element
testCaseToJUnit suiteName result = defElement
  { elName = QName "testcase" Nothing Nothing
  , elAttribs = [ Attr (QName "name" Nothing Nothing) (testName $ resultTestInfo result)
                , Attr (QName "classname" Nothing Nothing) suiteName
                , Attr (QName "time" Nothing Nothing) (show $ resultTime result)
                ]
  , elContent = if resultSuccessful result
                  then [ Elem propertiesElement ]
                  else [ Elem failureElement
                       , Elem propertiesElement ]
  }
  where
    propertiesElement= defElement
      { elName = QName "properties" Nothing Nothing
      , elAttribs = []
      , elContent = [Elem descriptionProperty]
      }
    descriptionProperty = defElement
      { elName = QName "property" Nothing Nothing
      , elAttribs = [ Attr (QName "name" Nothing Nothing) "description"
                    , Attr (QName "value" Nothing Nothing) (testDescription $ resultTestInfo result) ]
      , elContent = []
      }

    failureElement = defElement -- TODO: make framework handle test failures
      { elName = QName "failure" Nothing Nothing
      , elAttribs = [Attr (QName "message" Nothing Nothing) "test failure"] -- example type
      , elContent = [Text $ CData CDataText (fromJust (resultFailure result)) Nothing]
      }
