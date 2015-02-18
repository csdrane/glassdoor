{-# LANGUAGE OverloadedStrings #-}

module Replaceme where

import qualified Data.ByteString.Lazy.Internal as BL
import Control.Applicative ((<$>), (<*>))
import Control.Lens ((&), (.~), (^..))
import Control.Monad (liftM)
import Network.Wreq
import Data.Aeson.Lens (_Double, _Integer, _String, key, nth)
import Data.Aeson.Types ()
import Data.Text (pack, unpack)
import Text.PrettyPrint
import Safe (headMay)

data Company = Company { company :: String
                       , industry :: String
                       , numberofratings :: Integer
                       , overallrating :: Double
                       , compandbenefitsrating :: String
                       , careeropportunitiesrating :: String
                       , cultureandvaluesrating :: String
                       , recommendfriendrating :: String
                       , seniorleadershiprating :: String
                       , worklifebalancerating :: String
                       , website :: String } 
             deriving (Read, Show)

printCo :: Maybe Company -> IO ()
printCo (Just c) = (putStrLn . render . pretty) c
printCo Nothing = putStrLn "\nNothing"

pretty :: Company -> Doc
pretty (Company company' industry' numberofratings' overallrating' 
        compandbenefitsrating' careeropportunitiesrating' cultureandvaluesrating' 
        recommendfriendrating' seniorleadershiprating' worklifebalancerating'
        website') =
  cat [text "",
       text company' <+> text "(" <> text website' <> ")" <+> (" industry:" <+> text industry'),
       "Number of Ratings:" <+> integer numberofratings',
       "Overall:" <+> double  overallrating',
       "Comp and Benefits:" <+>  text compandbenefitsrating',
       "Career Opportunities:" <+> text careeropportunitiesrating',
       "Cultures and Values:" <+> text cultureandvaluesrating',
       "Recommend to a friend?" <+> text recommendfriendrating',
       "Senior Leadership:" <+> text seniorleadershiprating',
       "Work / Life Balance:" <+> text worklifebalancerating']
  

parseCompany :: Response BL.ByteString -> Maybe Company
parseCompany r = Company <$> company' <*> industry' <*> numberofratings' <*> 
                 overallrating' <*> compandbenefitsrating' <*> 
                 careeropportunitiesrating' <*> cultureandvaluesrating' <*>
                 recommendfriendrating' <*> seniorleadershiprating' <*>
                 worklifebalancerating' <*> website'
  where company' = extractString' "name" r
        industry' = extractString' "industry" r
        numberofratings' = extractInteger' "numberOfRatings" r
        overallrating' = extractDouble'  "overallRating" r
        compandbenefitsrating' = extractString' "compensationAndBenefitsRating" r
        careeropportunitiesrating' = extractString' "careerOpportunitiesRating" r
        cultureandvaluesrating' = extractString' "cultureAndValuesRating" r
        recommendfriendrating' = extractString' "recommendToFriendRating" r
        seniorleadershiprating' = extractString' "seniorLeadershipRating" r
        worklifebalancerating' = extractString' "workLifeBalanceRating" r
        website' = extractString' "website" r

extractString' :: String -> Response BL.ByteString -> Maybe String
extractString' s r = (headMay . map unpack) $ 
                       r ^.. responseBody . key (pack "response") 
                         . key (pack "employers") 
                         . nth 0
                         . key (pack s) 
                         . _String

extractDouble' :: String -> Response BL.ByteString -> Maybe Double
extractDouble' s r = headMay $ 
                       r ^.. responseBody . key (pack "response") 
                         . key (pack "employers") 
                         . nth 0
                         . key (pack s) 
                         . _Double

extractInteger' :: String -> Response BL.ByteString -> Maybe Integer
extractInteger' s r = headMay $ 
                       r ^.. responseBody . key (pack "response") 
                         . key (pack "employers") 
                         . nth 0
                         . key (pack s) 
                         . _Integer

readCompanies :: IO [String]
readCompanies = liftM (drop 2 . lines) (readFile companiesFile)
  where companiesFile = "/Users/chris/Programming/haskell/glassdoor/companies.txt"

queryCompany :: String -> IO (Response BL.ByteString)
queryCompany = lookup' . opts
  where lookup' o = getWith o "http://api.glassdoor.com/api/api.htm"

opts :: String -> Options
opts c = defaults 
         & param "v" .~ ["1"]
         & param "t.p" .~ ["30064"] 
         & param "t.k" .~ ["yy4pCnVdPs"] 
         & param "format" .~ ["json"] 
         & param "userip" .~ ["192.0.0.1"] 
         & param "useragent" .~ ["haskell wreq-0.3.0.1"]
         & param "action" .~ ["employers"]
         & param "q" .~ [pack c]

main :: IO ()
main = do
  cs <- readCompanies
  rs <- mapM queryCompany cs
  let ps = map parseCompany rs
  mapM_ printCo ps
    
