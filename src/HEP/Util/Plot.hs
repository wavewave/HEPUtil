{-# OPTIONS_GHC -fglasgow-exts #-}

module LHCOAnalysis.Analysis.Plot where

import HROOT

import System.IO
import Foreign.C
import Foreign.C.String
import Foreign.ForeignPtr


data PlotFileType = EPSFile | PDFFile

data HistDecoration = HistDecoration 
                      { histtitle    :: String
                      , xaxistitle   :: String
                      , yaxistitle   :: String 
                      }
                      
                      
plotHist :: PlotFileType -> HistName -> FilePath -> FilePath -> HistDecoration 
            -> IO CInt
plotHist plottype hname infile outfile hdeco = do 
  c_hname   <- newCString hname
  c_infile  <- newCString infile 
  c_outfile <- newCString outfile
  c_histtitle  <- newCString $ histtitle hdeco
  c_xaxistitle <- newCString $ xaxistitle hdeco
  c_yaxistitle <- newCString $ yaxistitle hdeco
  c_null    <- newCString ""
  let c_zero = fromIntegral 0
  c_plothist c_hname c_infile c_outfile 
    c_histtitle c_xaxistitle c_yaxistitle c_zero
    
read_histogram_from_file :: FilePath -> String -> IO TH1F
read_histogram_from_file filename histname 
  = do c_filename <- newCString filename
       c_histname <- newCString histname 
       rptr <- c_read_histogram_from_file c_filename c_histname
       fptr <- newForeignPtr_ rptr
       return $ TH1F fptr
       
       
       
{- write_histogram_to_file :: TH1F -> FilePath -> IO ()
write_histogram_to_file (TH1F fptr) filename 
  = do c_filename <- newCString filename
       let rptr = unsafeForeignPtrToPtr fptr
       c_write_histogram_to_file rptr c_filename -}


th1f_fill :: TH1F -> Double -> IO () 
th1f_fill (TH1F fptr) val 
  = do let c_val = realToFrac val 
           rptr = unsafeForeignPtrToPtr fptr

       c_th1f_fill rptr c_val 
       return ()

makePicsFromHist (TH1F fptr) outfile hdeco 
  = do c_outfile <- newCString outfile
       c_title  <- newCString $ histtitle hdeco
       c_xtitle <- newCString $ xaxistitle hdeco
       c_ytitle <- newCString $ yaxistitle hdeco
       let rptr = unsafeForeignPtrToPtr fptr
           
       c_make_pics_from_hist rptr c_outfile c_title c_xtitle c_ytitle