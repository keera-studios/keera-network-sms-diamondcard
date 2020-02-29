-- | This module provides two elements:
-- * An operation to contact DiamondCard and send SMSs using their website
-- * A datatype to process the result.
--
-- With the current implementation, the result page is not parsed.  Since the
-- SOAP interface is not used either, we don't know if the message will be
-- delivered correctly. We only know if the page loaded.
--
-- This issue is reflected in the datatype, which offers two constructors:
-- SMSNotSent (which reflects the fact that we know for sure that the message
-- will not be delivered) and SMSApparentlySent (which reflects that it might,
-- it might not be sent).
--
-- Copyright   : (C) Ivan Perez Dominguez, 2011
-- License     : BSD3
-- Maintainer  : support@keera.co.uk

module Network.SMS.DiamondCard
  ( SMSDeliveryResult(..)
  , sendSMS
  )
 where

-- External imports
import Data.IORef
-- import Data.List
import Network.Curl

-- | The result of sending an SMS. Since errors could be reported as part of the page
-- text, it's difficult to tell if something went wrong. The possible output
-- states are that a message might have been sent, or that it definitely wasn't.
data SMSDeliveryResult = SMSApparentlySent
                       | SMSNotSent String
 deriving (Eq)

-- | Send an sms using diamondcard's web interface.
-- The parameters are:
-- accountID
-- pinCode
-- message text (don't escape double quotes)
-- from number (with international code and without plus sign)
-- to number list (with international codes and no plus signs)
sendSMS :: String -> String -> String -> String -> [ String ] -> IO SMSDeliveryResult
sendSMS accID pinCode msg from tos = do

  -- Initialise a curl op
  h   <- initialize

  -- Post output as a list of strings
  ref <- newIORef []

  -- Add post params, url and output collector
  setopts h [ CurlVerbose False, CurlPostFields message
            , CurlURL url, CurlWriteFunction (gatherOutput ref)
            ]

  -- Gather the results
  rc  <- perform h
  lss <- readIORef ref

  return $ case rc of
             CurlOK -> SMSApparentlySent
             _      -> SMSNotSent $ concat $ reverse lss

  where url = "https://www.diamondcard.us/exec/voip-login" -- ++ params
        -- params  = concat (intersperse "&" message)
        message = [ "accId=" ++ accID
                  , "pinCode=" ++ pinCode
                  , "act=sms"
                  , "msg=" ++ msg
                  , "from=" ++ from
                  ] ++ to
        to = map ("to=" ++) tos
