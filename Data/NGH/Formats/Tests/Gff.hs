{-# LANGUAGE TemplateHaskell, OverloadedStrings #-}
module Data.NGH.Formats.Tests.Gff
    ( tests ) where

import Test.Framework.TH
import Test.HUnit
import Test.QuickCheck
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2

import Data.Maybe
import Data.NGH.Annotation
import Data.NGH.Formats.Gff

import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Char8 as L8

test_file = "\
    \##gff-version 3\n\
    \##sequence-region Pf3D7_01_v3 1 640851\n\
    \Pf3D7_01_v3\tchado\trepeat_region\t1\t360\t.\t+\t.\tID=Pfalciparum_REP_20;isObsolete=false;feature_id=20734007;timelastmodified=07.09.2011+06:34:16+BST\n\
    \Pf3D7_01_v3\tchado\trepeat_region\t361\t1418\t.\t+\t.\tID=Pfalciparum_REP_15;isObsolete=false;feature_id=20734008;timelastmodified=07.09.2011+06:34:16+BST\n\
    \Pf3D7_01_v3\tchado\trepeat_region\t2160\t3858\t.\t+\t.\tID=Pfalciparum_REP_35;isObsolete=false;feature_id=20734009;timelastmodified=07.09.2011+06:34:16+BST\n\
    \Pf3D7_01_v3\tchado\trepeat_region\t8856\t9021\t.\t+\t.\tID=Pfalciparum_REP_5;isObsolete=false;feature_id=20734010;timelastmodified=07.09.2011+06:34:16+BST\n\
    \Pf3D7_01_v3\tchado\trepeat_region\t9313\t9529\t.\t+\t.\tID=Pfalciparum_REP_25;isObsolete=false;feature_id=20734011;timelastmodified=07.09.2011+06:34:16+BST\n\
    \Pf3D7_01_v3\tchado\trepeat_region\t11315\t27336\t.\t+\t.\tID=Pfalciparum_REP_55;isObsolete=false;feature_id=20734013;timelastmodified=07.09.2011+06:34:17+BST\n\
    \Pf3D7_01_v3\tchado\tgene\t29510\t37126\t.\t+\t.\tID=PF3D7_0100100;Name=VAR;isObsolete=false;feature_id=20734001;timelastmodified=07.09.2011+06:34:15+BST\n\
    \Pf3D7_01_v3\tchado\tCDS\t29510\t34762\t.\t+\t.\tID=PF3D7_0100100.1:exon:1;Parent=PF3D7_0100100.1;isObsolete=false;timelastmodified=07.09.2011+06:34:18+BST\n\
    \Pf3D7_01_v3\tchado\tCDS\t35888\t37126\t.\t+\t.\tID=PF3D7_0100100.1:exon:2;Parent=PF3D7_0100100.1;isObsolete=false;timelastmodified=07.09.2011+06:34:18+BST\n\
    \Pf3D7_01_v3\tchado\tmRNA\t29510\t37126\t.\t+\t.\tID=PF3D7_0100100.1;Parent=PF3D7_0100100;isObsolete=false;feature_id=20731410;timelastmodified=07.09.2011+06:13:46+BST\n\
    \Pf3D7_01_v3\tchado\tpolypeptide\t29510\t37126\t.\t+\t.\tID=PF3D7_0100100.1:pep;Derives_from=PF3D7_0100100.1\n\
    \Pf3D7_01_v3\tchado\tpolypeptide_motif\t29568\t29581\t.\t+\t.\tID=chr01:motif:1;isObsolete=false;feature_id=20734020;literature=PMID:16507167\n\
    \Pf3D7_01_v3\tchado\tgene\t38982\t40207\t.\t-\t.\tID=PF3D7_0100200;Name=RIF;isObsolete=false;feature_id=20725865;timelastmodified=07.09.2011+05:28:48+BST\n\
    \Pf3D7_01_v3\tchado\tCDS\t40154\t40207\t.\t-\t.\tID=PF3D7_0100200.1:exon:2;Parent=PF3D7_0100200.1;isObsolete=false;timelastmodified=07.09.2011+06:34:19+BST\n\
    \Pf3D7_01_v3\tchado\tCDS\t38982\t39923\t.\t-\t.\tID=PF3D7_0100200.1:exon:1;Parent=PF3D7_0100200.1;isObsolete=false;timelastmodified=07.09.2011+06:34:19+BST\n\
    \Pf3D7_01_v3\tchado\tmRNA\t38982\t40207\t.\t-\t.\tID=PF3D7_0100200.1;Parent=PF3D7_0100200;isObsolete=false;feature_id=20725864;timelastmodified=07.09.2011+05:28:47+BST\n\
    \Pf3D7_01_v3\tchado\tpolypeptide\t38982\t40207\t.\t-\t.\tID=PF3D7_0100200.1:pep;Derives_from=PF3D7_0100200.1;Dbxref=UniProtKB:Q9NFB5%2CMPMP:cytoadherencescheme.html%2CMPMP:rosetting.html\n\
    \Pf3D7_01_v3\tchado\tpolypeptide_motif\t39847\t39860\t.\t-\t.\tID=chr01:motif:2;Parent=PF3D7_0100200.1;Note=PEXEL;isObsolete=false;timelastmodified=07.09.2011+06:34:21+BST\n\
    \Pf3D7_01_v3\tchado\tgene\t42367\t46507\t.\t-\t.\tID=PF3D7_0100300;isObsolete=false;feature_id=20725861;timelastmodified=07.09.2011+05:28:46+BST;previous_systematic_id=PFA0015c\n\
    \Pf3D7_01_v3\tchado\tCDS\t43775\t46507\t.\t-\t.\tID=PF3D7_0100300.1:exon:2;Parent=PF3D7_0100300.1;isObsolete=false;timelastmodified=07.09.2011+06:34:20+BST;colour=2"

annotations = readAnnotations test_file

tests = $(testGroupGenerator)

case_all = (length annotations) @?= ((length $ L8.lines test_file) - 2)
case_genes = (length genes) @?= 3
    where
        genes = filter ((== GffGene) . gffType) annotations


