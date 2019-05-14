(* Content-type: application/vnd.wolfram.cdf.text *)

(*** Wolfram CDF File ***)
(* http://www.wolfram.com/cdf *)

(* CreatedBy='Mathematica 11.0' *)

(*************************************************************************)
(*                                                                       *)
(*  The Mathematica License under which this file was created prohibits  *)
(*  restricting third parties in receipt of this file from republishing  *)
(*  or redistributing it by any means, including but not limited to      *)
(*  rights management or terms of use, without the express consent of    *)
(*  Wolfram Research, Inc. For additional information concerning CDF     *)
(*  licensing and redistribution see:                                    *)
(*                                                                       *)
(*        www.wolfram.com/cdf/adopting-cdf/licensing-options.html        *)
(*                                                                       *)
(*************************************************************************)

(*CacheID: 234*)
(* Internal cache information:
NotebookFileLineBreakTest
NotebookFileLineBreakTest
NotebookDataPosition[      1064,         20]
NotebookDataLength[    121558,       2872]
NotebookOptionsPosition[    121345,       2843]
NotebookOutlinePosition[    121831,       2863]
CellTagsIndexPosition[    121788,       2860]
WindowFrame->Normal*)

(* Beginning of Notebook Content *)
Notebook[{

Cell[CellGroupData[{
Cell[BoxData[{
 RowBox[{"Clear", "[", 
  RowBox[{"Evaluate", "[", 
   RowBox[{
    RowBox[{"Context", "[", "]"}], "<>", "\"\<*\>\""}], "]"}], 
  "]"}], "\[IndentingNewLine]", 
 RowBox[{
  RowBox[{"Kpm", " ", ":=", " ", 
   RowBox[{"{", 
    RowBox[{"kx", ",", " ", "ky", ",", " ", 
     RowBox[{"\[PlusMinus]", "kz1"}]}], "}"}]}], ";"}], "\[IndentingNewLine]", 
 RowBox[{
  RowBox[{"Kp", " ", ":=", " ", 
   RowBox[{"{", 
    RowBox[{"kx", ",", " ", "ky", ",", " ", "kz1"}], "}"}]}], 
  ";"}], "\[IndentingNewLine]", 
 RowBox[{
  RowBox[{"Km", " ", ":=", " ", 
   RowBox[{"{", 
    RowBox[{"kx", ",", " ", "ky", ",", " ", 
     RowBox[{"-", "kz1"}]}], "}"}]}], ";"}], "\[IndentingNewLine]", 
 RowBox[{
  RowBox[{"Z", " ", ":=", " ", 
   RowBox[{"{", 
    RowBox[{"0", ",", "0", ",", "1"}], "}"}]}], ";"}], "\[IndentingNewLine]", 
 RowBox[{
  RowBox[{"p", " ", ":=", " ", 
   RowBox[{"{", 
    RowBox[{"px", ",", "py", ",", "pz"}], "}"}]}], 
  ";"}], "\[IndentingNewLine]", 
 RowBox[{
  RowBox[{
   RowBox[{"m", ":=", 
    RowBox[{"{", 
     RowBox[{"mx", ",", "my", ",", "mz"}], "}"}]}], ";"}], 
  "\[IndentingNewLine]", 
  RowBox[{"(*", " ", 
   RowBox[{
    RowBox[{"Assumptions", ":", " ", 
     RowBox[{"kx", " ", "and", " ", "ky", " ", "are", " ", "real"}]}], ",", 
    " ", 
    RowBox[{
    "while", " ", "kt", " ", "and", " ", "k", " ", "are", " ", "real", " ", 
     "and", " ", "greater", " ", "than", " ", "zero"}]}], " ", 
   "*)"}]}], "\[IndentingNewLine]", 
 RowBox[{
  RowBox[{
   RowBox[{"$Assumptions", " ", "=", " ", 
    RowBox[{
     RowBox[{"Element", "[", 
      RowBox[{
       RowBox[{
       "kx", "|", "ky", " ", "|", " ", "x", " ", "|", " ", "y", " ", "|", " ",
         "z", " ", "|", " ", "\[Theta]"}], ",", "Reals"}], "]"}], " ", "&&", 
     "  ", 
     RowBox[{"kt", ">", "0"}], " ", "&&", " ", 
     RowBox[{"k1", ">", "0"}], " ", "&&", " ", 
     RowBox[{"k2", ">", "0"}], " ", "&&", 
     RowBox[{"\[Rho]", ">", "0"}], " ", "&&", " ", 
     RowBox[{"\[Epsilon]", ">", "0"}], " ", "&&", " ", 
     RowBox[{"dkx", ">", "0"}], " ", "&&", " ", 
     RowBox[{"dky", ">", "0"}], " ", "&&", " ", 
     RowBox[{"dkt", ">", "0"}], " ", "&&", " ", 
     RowBox[{"d\[Alpha]", ">", "0"}], " ", "&&", " ", 
     RowBox[{"\[Mu]2", ">", "0"}], " ", "&&", " ", 
     RowBox[{"\[Mu]1", ">", "0"}]}]}], ";"}], "\[IndentingNewLine]", 
  RowBox[{"(*", " ", 
   RowBox[{"'", 
    RowBox[{"Unit", "'"}], " ", "Vectors"}], " ", 
   "*)"}]}], "\[IndentingNewLine]", 
 RowBox[{
  RowBox[{"es", " ", "=", " ", 
   RowBox[{
    RowBox[{"Cross", "[", 
     RowBox[{"Z", ",", "Kp"}], "]"}], "/", 
    RowBox[{"(", "kt", ")"}]}]}], ";"}], "\[IndentingNewLine]", 
 RowBox[{
  RowBox[{"eppm", " ", "=", " ", 
   RowBox[{
    RowBox[{"Cross", "[", 
     RowBox[{
      RowBox[{"Cross", "[", 
       RowBox[{"Z", ",", "Kpm"}], "]"}], ",", "Kpm"}], "]"}], "/", 
    RowBox[{"(", 
     RowBox[{"k1", " ", "kt"}], ")"}]}]}], " ", ";"}], "\[IndentingNewLine]", 
 RowBox[{
  RowBox[{"epp", "=", " ", 
   RowBox[{
    RowBox[{"Cross", "[", 
     RowBox[{
      RowBox[{"Cross", "[", 
       RowBox[{"Z", ",", "Kp"}], "]"}], ",", "Kp"}], "]"}], "/", 
    RowBox[{"(", 
     RowBox[{"k1", " ", "kt"}], ")"}]}]}], " ", ";"}], "\[IndentingNewLine]", 
 RowBox[{
  RowBox[{"epm", "=", " ", 
   RowBox[{
    RowBox[{"Cross", "[", 
     RowBox[{
      RowBox[{"Cross", "[", 
       RowBox[{"Z", ",", "Km"}], "]"}], ",", "Km"}], "]"}], "/", 
    RowBox[{"(", 
     RowBox[{"k1", " ", "kt"}], ")"}]}]}], " ", ";"}], "\[IndentingNewLine]", 
 RowBox[{
  RowBox[{"EpEpp", "=", 
   RowBox[{
    FractionBox["\[ImaginaryI]", 
     RowBox[{"2", "\[Pi]"}]], 
    FractionBox["1", 
     RowBox[{"4", "\[Pi]", " "}]], 
    FractionBox[
     RowBox[{"k1", "^", "2"}], 
     RowBox[{"kz1", "  "}]], 
    RowBox[{"(", 
     RowBox[{
      RowBox[{"Dot", "[", 
       RowBox[{"epp", ",", " ", "p"}], "]"}], " ", "+", 
      RowBox[{
       FractionBox["1", "c"], 
       RowBox[{"(", 
        RowBox[{"Dot", "[", 
         RowBox[{"es", ",", "m"}], "]"}], ")"}]}]}], ")"}]}]}], 
  ";"}], "\[IndentingNewLine]", 
 RowBox[{
  RowBox[{"EpEpm", "=", 
   RowBox[{
    FractionBox["\[ImaginaryI]", 
     RowBox[{"2", "\[Pi]"}]], 
    FractionBox["1", 
     RowBox[{"4", "\[Pi]", " "}]], 
    FractionBox[
     RowBox[{"k1", "^", "2"}], 
     RowBox[{"kz1", "  "}]], 
    RowBox[{"(", 
     RowBox[{
      RowBox[{"Dot", "[", 
       RowBox[{"epm", ",", " ", "p"}], "]"}], " ", "+", 
      RowBox[{
       FractionBox["1", "c"], 
       RowBox[{"(", 
        RowBox[{"Dot", "[", 
         RowBox[{"es", ",", "m"}], "]"}], ")"}]}]}], ")"}]}]}], 
  ";"}], "\[IndentingNewLine]", 
 RowBox[{
  RowBox[{"EsEpp", "=", 
   RowBox[{
    FractionBox["\[ImaginaryI]", 
     RowBox[{"2", "\[Pi]"}]], 
    FractionBox["1", 
     RowBox[{"4", "\[Pi]", " "}]], 
    FractionBox[
     RowBox[{"k1", "^", "2"}], 
     RowBox[{"kz1", "  "}]], 
    RowBox[{"(", 
     RowBox[{
      RowBox[{"Dot", "[", 
       RowBox[{"es", ",", "p"}], "]"}], "-", 
      RowBox[{
       FractionBox["1", "c"], 
       RowBox[{"(", 
        RowBox[{"Dot", "[", 
         RowBox[{"epp", ",", "m"}], "]"}], ")"}]}]}], ")"}]}]}], ";"}], "\n", 
 RowBox[{
  RowBox[{"EsEpm", "=", 
   RowBox[{
    FractionBox["\[ImaginaryI]", 
     RowBox[{"2", "\[Pi]"}]], 
    FractionBox["1", 
     RowBox[{"4", "\[Pi]", " "}]], 
    FractionBox[
     RowBox[{"k1", "^", "2"}], 
     RowBox[{"kz1", "  "}]], 
    RowBox[{"(", 
     RowBox[{
      RowBox[{"Dot", "[", 
       RowBox[{"es", ",", "p"}], "]"}], "-", 
      RowBox[{
       FractionBox["1", "c"], 
       RowBox[{"(", 
        RowBox[{"Dot", "[", 
         RowBox[{"epm", ",", "m"}], "]"}], ")"}]}]}], ")"}]}]}], 
  ";"}], "\[IndentingNewLine]", 
 RowBox[{
  RowBox[{"kz1", " ", "=", " ", 
   SqrtBox[
    RowBox[{
     SuperscriptBox["k1", "2"], "-", 
     SuperscriptBox["kt", "2"]}]]}], ";"}], "\[IndentingNewLine]", 
 RowBox[{
  RowBox[{"kt", " ", "=", " ", 
   SqrtBox[
    RowBox[{
     SuperscriptBox["kx", "2"], "+", 
     SuperscriptBox["ky", "2"]}]]}], ";"}], "\[IndentingNewLine]", 
 RowBox[{
  RowBox[{
   RowBox[{"k1", " ", "=", " ", "1"}], ";"}], "\[IndentingNewLine]", 
  RowBox[{"(*", 
   RowBox[{
    RowBox[{"solinit", "=", " ", 
     RowBox[{"{", 
      RowBox[{"0", ",", "0", ",", "0", ",", "0", ",", "0", ",", "0"}], 
      "}"}]}], ";", "\[IndentingNewLine]", 
    RowBox[{"nulinit", " ", "=", " ", 
     DynamicBox[
      ToBoxes[$CellContext`nulsol = 
       NullSpace[{$CellContext`M1, $CellContext`M2, $CellContext`M3, \
$CellContext`M4, $CellContext`M5, $CellContext`M6}], StandardForm],
      ImageSizeCache->{750.8, {9.9, 20.5}}]}], ";", "\[IndentingNewLine]", 
    RowBox[{"Dynamic", "[", 
     RowBox[{"If", "[", 
      RowBox[{
       RowBox[{
        RowBox[{"sol", "\[Equal]", "solinit"}], "&&", 
        RowBox[{"nulsol", "\[Equal]", "nulinit"}]}], ",", 
       "\[IndentingNewLine]", 
       RowBox[{
        RowBox[{"coef", "=", "\"\< \>\""}], ";", "\[IndentingNewLine]", 
        RowBox[{"plus", " ", "=", " ", "\"\< \>\""}], ";", 
        "\[IndentingNewLine]", 
        RowBox[{"with", " ", "=", " ", "\"\< \>\""}], ";", 
        "\[IndentingNewLine]", 
        RowBox[{"compl", " ", "=", " ", "\"\< \>\""}], ";", 
        "\[IndentingNewLine]", 
        RowBox[{"ToPrint", "=", "\"\< \>\""}], ";"}]}], "\[IndentingNewLine]",
       "]"}], "]"}]}], "*)"}]}], "\[IndentingNewLine]", 
 RowBox[{"Dynamic", "[", 
  RowBox[{
   RowBox[{"If", " ", "[", 
    RowBox[{
     RowBox[{"point1", "\[Equal]", "False"}], ",", "\[IndentingNewLine]", 
     RowBox[{"Dynamic", "[", 
      RowBox[{
       RowBox[{"Which", "[", "\[IndentingNewLine]", 
        RowBox[{
         RowBox[{
          RowBox[{"pol1", "\[Equal]", "\"\<spol\>\""}], "&&", 
          RowBox[{"plane1", "\[Equal]", "\"\<zplus\>\""}]}], ",", 
         "\[IndentingNewLine]", 
         RowBox[{"M1", "=", 
          RowBox[{"Coefficient", "[", 
           RowBox[{
            RowBox[{"EsEpp", "/.", 
             RowBox[{"{", 
              RowBox[{
               RowBox[{"kx", "\[Rule]", "kx1"}], ",", 
               RowBox[{"ky", "\[Rule]", "ky1"}]}], "}"}]}], ",", 
            RowBox[{"{", 
             RowBox[{
             "px", ",", "py", ",", "pz", ",", "mx", ",", "my", ",", "mz"}], 
             "}"}]}], "]"}]}], ",", "\[IndentingNewLine]", 
         RowBox[{
          RowBox[{"pol1", " ", "\[Equal]", "\"\<spol\>\""}], "&&", 
          RowBox[{"plane1", "\[Equal]", "\"\<zmin\>\""}]}], ",", 
         "\[IndentingNewLine]", 
         RowBox[{"M1", "=", 
          RowBox[{"Coefficient", "[", 
           RowBox[{
            RowBox[{"EsEpm", "/.", 
             RowBox[{"{", 
              RowBox[{
               RowBox[{"kx", "\[Rule]", "kx1"}], ",", 
               RowBox[{"ky", "\[Rule]", "ky1"}]}], "}"}]}], ",", 
            RowBox[{"{", 
             RowBox[{
             "px", ",", "py", ",", "pz", ",", "mx", ",", "my", ",", "mz"}], 
             "}"}]}], "]"}]}], ",", "\[IndentingNewLine]", 
         RowBox[{
          RowBox[{"pol1", " ", "\[Equal]", "\"\<ppol\>\""}], "&&", 
          RowBox[{"plane1", "\[Equal]", "\"\<zplus\>\""}]}], ",", 
         "\[IndentingNewLine]", 
         RowBox[{"M1", "=", 
          RowBox[{"Coefficient", "[", 
           RowBox[{
            RowBox[{"EpEpp", "/.", 
             RowBox[{"{", 
              RowBox[{
               RowBox[{"kx", "\[Rule]", "kx1"}], ",", 
               RowBox[{"ky", "\[Rule]", "ky1"}]}], "}"}]}], ",", 
            RowBox[{"{", 
             RowBox[{
             "px", ",", "py", ",", "pz", ",", "mx", ",", "my", ",", "mz"}], 
             "}"}]}], "]"}]}], ",", "\[IndentingNewLine]", 
         RowBox[{
          RowBox[{"pol1", " ", "\[Equal]", "\"\<ppol\>\""}], "&&", " ", 
          RowBox[{"plane1", "\[Equal]", "\"\<zmin\>\""}]}], ",", 
         "\[IndentingNewLine]", 
         RowBox[{"M1", "=", 
          RowBox[{"Coefficient", "[", 
           RowBox[{
            RowBox[{"EpEpm", "/.", 
             RowBox[{"{", 
              RowBox[{
               RowBox[{"kx", "\[Rule]", "kx1"}], ",", 
               RowBox[{"ky", "\[Rule]", "ky1"}]}], "}"}]}], ",", 
            RowBox[{"{", 
             RowBox[{
             "px", ",", "py", ",", "pz", ",", "mx", ",", "my", ",", "mz"}], 
             "}"}]}], "]"}]}]}], "]"}], "\[IndentingNewLine]", ",", 
       RowBox[{"SaveDefinitions", "\[Rule]", "True"}]}], "]"}], ",", 
     RowBox[{
      RowBox[{"M1", "=", 
       RowBox[{"{", 
        RowBox[{"0", ",", "0", ",", "0", ",", "0", ",", "0", ",", "0"}], 
        "}"}]}], ";", 
      RowBox[{"reA1", "=", "0"}], ";", 
      RowBox[{"imA1", "=", "0"}]}]}], "]"}], ",", 
   RowBox[{"SaveDefinitions", "\[Rule]", "True"}]}], 
  "]"}], "\[IndentingNewLine]", 
 RowBox[{"Dynamic", "[", 
  RowBox[{
   RowBox[{"If", " ", "[", 
    RowBox[{
     RowBox[{"point2", "\[Equal]", "False"}], ",", "\[IndentingNewLine]", 
     RowBox[{"Dynamic", "[", 
      RowBox[{
       RowBox[{"Which", "[", "\[IndentingNewLine]", 
        RowBox[{
         RowBox[{
          RowBox[{"pol2", "\[Equal]", "\"\<spol\>\""}], "&&", 
          RowBox[{"plane2", "\[Equal]", "\"\<zplus\>\""}]}], ",", 
         "\[IndentingNewLine]", 
         RowBox[{"M2", "=", 
          RowBox[{"Coefficient", "[", 
           RowBox[{
            RowBox[{"EsEpp", "/.", 
             RowBox[{"{", 
              RowBox[{
               RowBox[{"kx", "\[Rule]", "kx2"}], ",", 
               RowBox[{"ky", "\[Rule]", "ky2"}]}], "}"}]}], ",", 
            RowBox[{"{", 
             RowBox[{
             "px", ",", "py", ",", "pz", ",", "mx", ",", "my", ",", "mz"}], 
             "}"}]}], "]"}]}], ",", "\[IndentingNewLine]", 
         RowBox[{
          RowBox[{"pol2", " ", "\[Equal]", "\"\<spol\>\""}], "&&", 
          RowBox[{"plane2", "\[Equal]", "\"\<zmin\>\""}]}], ",", 
         "\[IndentingNewLine]", 
         RowBox[{"M2", "=", 
          RowBox[{"Coefficient", "[", 
           RowBox[{
            RowBox[{"EsEpm", "/.", 
             RowBox[{"{", 
              RowBox[{
               RowBox[{"kx", "\[Rule]", "kx2"}], ",", 
               RowBox[{"ky", "\[Rule]", "ky2"}]}], "}"}]}], ",", 
            RowBox[{"{", 
             RowBox[{
             "px", ",", "py", ",", "pz", ",", "mx", ",", "my", ",", "mz"}], 
             "}"}]}], "]"}]}], ",", "\[IndentingNewLine]", 
         RowBox[{
          RowBox[{"pol2", " ", "\[Equal]", "\"\<ppol\>\""}], "&&", 
          RowBox[{"plane2", "\[Equal]", "\"\<zplus\>\""}]}], ",", 
         "\[IndentingNewLine]", 
         RowBox[{"M2", "=", 
          RowBox[{"Coefficient", "[", 
           RowBox[{
            RowBox[{"EpEpp", "/.", 
             RowBox[{"{", 
              RowBox[{
               RowBox[{"kx", "\[Rule]", "kx2"}], ",", 
               RowBox[{"ky", "\[Rule]", "ky2"}]}], "}"}]}], ",", 
            RowBox[{"{", 
             RowBox[{
             "px", ",", "py", ",", "pz", ",", "mx", ",", "my", ",", "mz"}], 
             "}"}]}], "]"}]}], ",", "\[IndentingNewLine]", 
         RowBox[{
          RowBox[{"pol2", " ", "\[Equal]", "\"\<ppol\>\""}], "&&", " ", 
          RowBox[{"plane2", "\[Equal]", "\"\<zmin\>\""}]}], ",", 
         "\[IndentingNewLine]", 
         RowBox[{"M2", "=", 
          RowBox[{"Coefficient", "[", 
           RowBox[{
            RowBox[{"EpEpm", "/.", 
             RowBox[{"{", 
              RowBox[{
               RowBox[{"kx", "\[Rule]", "kx2"}], ",", 
               RowBox[{"ky", "\[Rule]", "ky2"}]}], "}"}]}], ",", 
            RowBox[{"{", 
             RowBox[{
             "px", ",", "py", ",", "pz", ",", "mx", ",", "my", ",", "mz"}], 
             "}"}]}], "]"}]}]}], "]"}], "\[IndentingNewLine]", ",", 
       RowBox[{"SaveDefinitions", "\[Rule]", "True"}]}], "]"}], ",", 
     RowBox[{
      RowBox[{"M2", "=", 
       RowBox[{"{", 
        RowBox[{"0", ",", "0", ",", "0", ",", "0", ",", "0", ",", "0"}], 
        "}"}]}], ";", 
      RowBox[{"reA2", "=", "0"}], ";", 
      RowBox[{"imA2", "=", "0"}]}]}], "]"}], ",", 
   RowBox[{"SaveDefinitions", "\[Rule]", "True"}]}], 
  "]"}], "\[IndentingNewLine]", 
 RowBox[{"Dynamic", "[", 
  RowBox[{
   RowBox[{"If", " ", "[", 
    RowBox[{
     RowBox[{"point3", "\[Equal]", "False"}], ",", "\[IndentingNewLine]", 
     RowBox[{"Dynamic", "[", 
      RowBox[{
       RowBox[{"Which", "[", "\[IndentingNewLine]", 
        RowBox[{
         RowBox[{
          RowBox[{"pol3", "\[Equal]", "\"\<spol\>\""}], "&&", 
          RowBox[{"plane3", "\[Equal]", "\"\<zplus\>\""}]}], ",", 
         "\[IndentingNewLine]", 
         RowBox[{"M3", "=", 
          RowBox[{"Coefficient", "[", 
           RowBox[{
            RowBox[{"EsEpp", "/.", 
             RowBox[{"{", 
              RowBox[{
               RowBox[{"kx", "\[Rule]", "kx3"}], ",", 
               RowBox[{"ky", "\[Rule]", "ky3"}]}], "}"}]}], ",", 
            RowBox[{"{", 
             RowBox[{
             "px", ",", "py", ",", "pz", ",", "mx", ",", "my", ",", "mz"}], 
             "}"}]}], "]"}]}], ",", "\[IndentingNewLine]", 
         RowBox[{
          RowBox[{"pol3", " ", "\[Equal]", "\"\<spol\>\""}], "&&", 
          RowBox[{"plane3", "\[Equal]", "\"\<zmin\>\""}]}], ",", 
         "\[IndentingNewLine]", 
         RowBox[{"M3", "=", 
          RowBox[{"Coefficient", "[", 
           RowBox[{
            RowBox[{"EsEpm", "/.", 
             RowBox[{"{", 
              RowBox[{
               RowBox[{"kx", "\[Rule]", "kx3"}], ",", 
               RowBox[{"ky", "\[Rule]", "ky3"}]}], "}"}]}], ",", 
            RowBox[{"{", 
             RowBox[{
             "px", ",", "py", ",", "pz", ",", "mx", ",", "my", ",", "mz"}], 
             "}"}]}], "]"}]}], ",", "\[IndentingNewLine]", 
         RowBox[{
          RowBox[{"pol3", " ", "\[Equal]", "\"\<ppol\>\""}], "&&", 
          RowBox[{"plane3", "\[Equal]", "\"\<zplus\>\""}]}], ",", 
         "\[IndentingNewLine]", 
         RowBox[{"M3", "=", 
          RowBox[{"Coefficient", "[", 
           RowBox[{
            RowBox[{"EpEpp", "/.", 
             RowBox[{"{", 
              RowBox[{
               RowBox[{"kx", "\[Rule]", "kx3"}], ",", 
               RowBox[{"ky", "\[Rule]", "ky3"}]}], "}"}]}], ",", 
            RowBox[{"{", 
             RowBox[{
             "px", ",", "py", ",", "pz", ",", "mx", ",", "my", ",", "mz"}], 
             "}"}]}], "]"}]}], ",", "\[IndentingNewLine]", 
         RowBox[{
          RowBox[{"pol3", " ", "\[Equal]", "\"\<ppol\>\""}], "&&", " ", 
          RowBox[{"plane3", "\[Equal]", "\"\<zmin\>\""}]}], ",", 
         "\[IndentingNewLine]", 
         RowBox[{"M3", "=", 
          RowBox[{"Coefficient", "[", 
           RowBox[{
            RowBox[{"EpEpm", "/.", 
             RowBox[{"{", 
              RowBox[{
               RowBox[{"kx", "\[Rule]", "kx3"}], ",", 
               RowBox[{"ky", "\[Rule]", "ky3"}]}], "}"}]}], ",", 
            RowBox[{"{", 
             RowBox[{
             "px", ",", "py", ",", "pz", ",", "mx", ",", "my", ",", "mz"}], 
             "}"}]}], "]"}]}]}], "]"}], "\[IndentingNewLine]", ",", 
       RowBox[{"SaveDefinitions", "\[Rule]", "True"}]}], "]"}], ",", 
     RowBox[{
      RowBox[{"M3", "=", 
       RowBox[{"{", 
        RowBox[{"0", ",", "0", ",", "0", ",", "0", ",", "0", ",", "0"}], 
        "}"}]}], ";", 
      RowBox[{"reA3", "=", "0"}]}], ",", 
     RowBox[{"imA3", "=", "0"}]}], "]"}], ",", 
   RowBox[{"SaveDefinitions", "\[Rule]", "True"}]}], 
  "]"}], "\[IndentingNewLine]", 
 RowBox[{"Dynamic", "[", 
  RowBox[{
   RowBox[{"If", " ", "[", 
    RowBox[{
     RowBox[{"point4", "\[Equal]", "False"}], ",", "\[IndentingNewLine]", 
     RowBox[{"Dynamic", "[", 
      RowBox[{
       RowBox[{"Which", "[", "\[IndentingNewLine]", 
        RowBox[{
         RowBox[{
          RowBox[{"pol4", "\[Equal]", "\"\<spol\>\""}], "&&", 
          RowBox[{"plane4", "\[Equal]", "\"\<zplus\>\""}]}], ",", 
         "\[IndentingNewLine]", 
         RowBox[{"M4", "=", 
          RowBox[{"Coefficient", "[", 
           RowBox[{
            RowBox[{"EsEpp", "/.", 
             RowBox[{"{", 
              RowBox[{
               RowBox[{"kx", "\[Rule]", "kx4"}], ",", 
               RowBox[{"ky", "\[Rule]", "ky4"}]}], "}"}]}], ",", 
            RowBox[{"{", 
             RowBox[{
             "px", ",", "py", ",", "pz", ",", "mx", ",", "my", ",", "mz"}], 
             "}"}]}], "]"}]}], ",", "\[IndentingNewLine]", 
         RowBox[{
          RowBox[{"pol4", " ", "\[Equal]", "\"\<spol\>\""}], "&&", 
          RowBox[{"plane4", "\[Equal]", "\"\<zmin\>\""}]}], ",", 
         "\[IndentingNewLine]", 
         RowBox[{"M4", "=", 
          RowBox[{"Coefficient", "[", 
           RowBox[{
            RowBox[{"EsEpm", "/.", 
             RowBox[{"{", 
              RowBox[{
               RowBox[{"kx", "\[Rule]", "kx4"}], ",", 
               RowBox[{"ky", "\[Rule]", "ky4"}]}], "}"}]}], ",", 
            RowBox[{"{", 
             RowBox[{
             "px", ",", "py", ",", "pz", ",", "mx", ",", "my", ",", "mz"}], 
             "}"}]}], "]"}]}], ",", "\[IndentingNewLine]", 
         RowBox[{
          RowBox[{"pol4", " ", "\[Equal]", "\"\<ppol\>\""}], "&&", 
          RowBox[{"plane4", "\[Equal]", "\"\<zplus\>\""}]}], ",", 
         "\[IndentingNewLine]", 
         RowBox[{"M4", "=", 
          RowBox[{"Coefficient", "[", 
           RowBox[{
            RowBox[{"EpEpp", "/.", 
             RowBox[{"{", 
              RowBox[{
               RowBox[{"kx", "\[Rule]", "kx4"}], ",", 
               RowBox[{"ky", "\[Rule]", "ky4"}]}], "}"}]}], ",", 
            RowBox[{"{", 
             RowBox[{
             "px", ",", "py", ",", "pz", ",", "mx", ",", "my", ",", "mz"}], 
             "}"}]}], "]"}]}], ",", "\[IndentingNewLine]", 
         RowBox[{
          RowBox[{"pol4", " ", "\[Equal]", "\"\<ppol\>\""}], "&&", " ", 
          RowBox[{"plane4", "\[Equal]", "\"\<zmin\>\""}]}], ",", 
         "\[IndentingNewLine]", 
         RowBox[{"M4", "=", 
          RowBox[{"Coefficient", "[", 
           RowBox[{
            RowBox[{"EpEpm", "/.", 
             RowBox[{"{", 
              RowBox[{
               RowBox[{"kx", "\[Rule]", "kx4"}], ",", 
               RowBox[{"ky", "\[Rule]", "ky4"}]}], "}"}]}], ",", 
            RowBox[{"{", 
             RowBox[{
             "px", ",", "py", ",", "pz", ",", "mx", ",", "my", ",", "mz"}], 
             "}"}]}], "]"}]}]}], "]"}], "\[IndentingNewLine]", ",", 
       RowBox[{"SaveDefinitions", "\[Rule]", "True"}]}], "]"}], ",", 
     RowBox[{
      RowBox[{"M4", "=", 
       RowBox[{"{", 
        RowBox[{"0", ",", "0", ",", "0", ",", "0", ",", "0", ",", "0"}], 
        "}"}]}], ";", 
      RowBox[{"reA4", "=", "0"}]}], ",", 
     RowBox[{"imA4", "=", "0"}]}], "]"}], ",", 
   RowBox[{"SaveDefinitions", "\[Rule]", "True"}]}], 
  "]"}], "\[IndentingNewLine]", 
 RowBox[{"Dynamic", "[", 
  RowBox[{
   RowBox[{"If", " ", "[", 
    RowBox[{
     RowBox[{"point5", "\[Equal]", "False"}], ",", "\[IndentingNewLine]", 
     RowBox[{"Dynamic", "[", 
      RowBox[{
       RowBox[{"Which", "[", "\[IndentingNewLine]", 
        RowBox[{
         RowBox[{
          RowBox[{"pol5", "\[Equal]", "\"\<spol\>\""}], "&&", 
          RowBox[{"plane5", "\[Equal]", "\"\<zplus\>\""}]}], ",", 
         "\[IndentingNewLine]", 
         RowBox[{"M5", "=", 
          RowBox[{"Coefficient", "[", 
           RowBox[{
            RowBox[{"EsEpp", "/.", 
             RowBox[{"{", 
              RowBox[{
               RowBox[{"kx", "\[Rule]", "kx5"}], ",", 
               RowBox[{"ky", "\[Rule]", "ky5"}]}], "}"}]}], ",", 
            RowBox[{"{", 
             RowBox[{
             "px", ",", "py", ",", "pz", ",", "mx", ",", "my", ",", "mz"}], 
             "}"}]}], "]"}]}], ",", "\[IndentingNewLine]", 
         RowBox[{
          RowBox[{"pol5", " ", "\[Equal]", "\"\<spol\>\""}], "&&", 
          RowBox[{"plane5", "\[Equal]", "\"\<zmin\>\""}]}], ",", 
         "\[IndentingNewLine]", 
         RowBox[{"M5", "=", 
          RowBox[{"Coefficient", "[", 
           RowBox[{
            RowBox[{"EsEpm", "/.", 
             RowBox[{"{", 
              RowBox[{
               RowBox[{"kx", "\[Rule]", "kx5"}], ",", 
               RowBox[{"ky", "\[Rule]", "ky5"}]}], "}"}]}], ",", 
            RowBox[{"{", 
             RowBox[{
             "px", ",", "py", ",", "pz", ",", "mx", ",", "my", ",", "mz"}], 
             "}"}]}], "]"}]}], ",", "\[IndentingNewLine]", 
         RowBox[{
          RowBox[{"pol5", "\[Equal]", "\"\<ppol\>\""}], "&&", 
          RowBox[{"plane5", "\[Equal]", "\"\<zplus\>\""}]}], ",", 
         "\[IndentingNewLine]", 
         RowBox[{"M5", "=", 
          RowBox[{"Coefficient", "[", 
           RowBox[{
            RowBox[{"EpEpp", "/.", 
             RowBox[{"{", 
              RowBox[{
               RowBox[{"kx", "\[Rule]", "kx5"}], ",", 
               RowBox[{"ky", "\[Rule]", "ky5"}]}], "}"}]}], ",", 
            RowBox[{"{", 
             RowBox[{
             "px", ",", "py", ",", "pz", ",", "mx", ",", "my", ",", "mz"}], 
             "}"}]}], "]"}]}], ",", "\[IndentingNewLine]", 
         RowBox[{
          RowBox[{"pol5", " ", "\[Equal]", "\"\<ppol\>\""}], "&&", " ", 
          RowBox[{"plane5", "\[Equal]", "\"\<zmin\>\""}]}], ",", 
         "\[IndentingNewLine]", 
         RowBox[{"M5", "=", 
          RowBox[{"Coefficient", "[", 
           RowBox[{
            RowBox[{"EpEpm", "/.", 
             RowBox[{"{", 
              RowBox[{
               RowBox[{"kx", "\[Rule]", "kx5"}], ",", 
               RowBox[{"ky", "\[Rule]", "ky5"}]}], "}"}]}], ",", 
            RowBox[{"{", 
             RowBox[{
             "px", ",", "py", ",", "pz", ",", "mx", ",", "my", ",", "mz"}], 
             "}"}]}], "]"}]}]}], "]"}], "\[IndentingNewLine]", ",", 
       RowBox[{"SaveDefinitions", "\[Rule]", "True"}]}], "]"}], ",", 
     RowBox[{
      RowBox[{"M5", "=", 
       RowBox[{"{", 
        RowBox[{"0", ",", "0", ",", "0", ",", "0", ",", "0", ",", "0"}], 
        "}"}]}], ";", 
      RowBox[{"reA5", "=", "0"}]}], ",", 
     RowBox[{"imA5", "=", "0"}]}], "]"}], ",", 
   RowBox[{"SaveDefinitions", "\[Rule]", "True"}]}], 
  "]"}], "\[IndentingNewLine]", 
 RowBox[{"Dynamic", "[", 
  RowBox[{
   RowBox[{"If", " ", "[", 
    RowBox[{
     RowBox[{"point6", "\[Equal]", "False"}], ",", "\[IndentingNewLine]", 
     RowBox[{"Dynamic", "[", 
      RowBox[{
       RowBox[{"Which", "[", "\[IndentingNewLine]", 
        RowBox[{
         RowBox[{
          RowBox[{"pol6", "\[Equal]", "\"\<spol\>\""}], "&&", 
          RowBox[{"plane6", "\[Equal]", "\"\<zplus\>\""}]}], ",", 
         "\[IndentingNewLine]", 
         RowBox[{"M6", "=", 
          RowBox[{"Coefficient", "[", 
           RowBox[{
            RowBox[{"EsEpp", "/.", 
             RowBox[{"{", 
              RowBox[{
               RowBox[{"kx", "\[Rule]", "kx6"}], ",", 
               RowBox[{"ky", "\[Rule]", "ky6"}]}], "}"}]}], ",", 
            RowBox[{"{", 
             RowBox[{
             "px", ",", "py", ",", "pz", ",", "mx", ",", "my", ",", "mz"}], 
             "}"}]}], "]"}]}], ",", "\[IndentingNewLine]", 
         RowBox[{
          RowBox[{"pol6", " ", "\[Equal]", "\"\<spol\>\""}], "&&", 
          RowBox[{"plane6", "\[Equal]", "\"\<zmin\>\""}]}], ",", 
         "\[IndentingNewLine]", 
         RowBox[{"M6", "=", 
          RowBox[{"Coefficient", "[", 
           RowBox[{
            RowBox[{"EsEpm", "/.", 
             RowBox[{"{", 
              RowBox[{
               RowBox[{"kx", "\[Rule]", "kx6"}], ",", 
               RowBox[{"ky", "\[Rule]", "ky6"}]}], "}"}]}], ",", 
            RowBox[{"{", 
             RowBox[{
             "px", ",", "py", ",", "pz", ",", "mx", ",", "my", ",", "mz"}], 
             "}"}]}], "]"}]}], ",", "\[IndentingNewLine]", 
         RowBox[{
          RowBox[{"pol6", " ", "\[Equal]", "\"\<ppol\>\""}], "&&", 
          RowBox[{"plane6", "\[Equal]", "\"\<zplus\>\""}]}], ",", 
         "\[IndentingNewLine]", 
         RowBox[{"M6", "=", 
          RowBox[{"Coefficient", "[", 
           RowBox[{
            RowBox[{"EpEpp", "/.", 
             RowBox[{"{", 
              RowBox[{
               RowBox[{"kx", "\[Rule]", "kx6"}], ",", 
               RowBox[{"ky", "\[Rule]", "ky6"}]}], "}"}]}], ",", 
            RowBox[{"{", 
             RowBox[{
             "px", ",", "py", ",", "pz", ",", "mx", ",", "my", ",", "mz"}], 
             "}"}]}], "]"}]}], ",", "\[IndentingNewLine]", 
         RowBox[{
          RowBox[{"pol6", " ", "\[Equal]", "\"\<ppol\>\""}], "&&", " ", 
          RowBox[{"plane6", "\[Equal]", "\"\<zmin\>\""}]}], ",", 
         "\[IndentingNewLine]", 
         RowBox[{"M6", "=", 
          RowBox[{"Coefficient", "[", 
           RowBox[{
            RowBox[{"EpEpm", "/.", 
             RowBox[{"{", 
              RowBox[{
               RowBox[{"kx", "\[Rule]", "kx6"}], ",", 
               RowBox[{"ky", "\[Rule]", "ky6"}]}], "}"}]}], ",", 
            RowBox[{"{", 
             RowBox[{
             "px", ",", "py", ",", "pz", ",", "mx", ",", "my", ",", "mz"}], 
             "}"}]}], "]"}]}]}], "]"}], "\[IndentingNewLine]", ",", 
       RowBox[{"SaveDefinitions", "\[Rule]", "True"}]}], "]"}], ",", 
     RowBox[{
      RowBox[{"M6", "=", 
       RowBox[{"{", 
        RowBox[{"0", ",", "0", ",", "0", ",", "0", ",", "0", ",", "0"}], 
        "}"}]}], ";", 
      RowBox[{"reA6", "=", "0"}], ";", 
      RowBox[{"imA6", "=", "0"}]}]}], "]"}], ",", 
   RowBox[{"SaveDefinitions", "\[Rule]", "True"}]}], 
  "]"}], "\[IndentingNewLine]", 
 RowBox[{"Dynamic", "[", 
  RowBox[{
   RowBox[{"sol", "=", 
    RowBox[{"LinearSolve", "[", 
     RowBox[{
      RowBox[{"{", 
       RowBox[{"M1", ",", "M2", ",", "M3", ",", "M4", ",", "M5", ",", "M6"}], 
       "}"}], ",", 
      RowBox[{
       RowBox[{"{", 
        RowBox[{
         RowBox[{"reA1", "+", 
          RowBox[{"\[ImaginaryI]", " ", "imA1"}]}], ",", 
         RowBox[{"reA2", "+", 
          RowBox[{"\[ImaginaryI]", " ", "imA2"}]}], ",", 
         RowBox[{"reA3", "+", 
          RowBox[{"\[ImaginaryI]", " ", "imA3"}]}], ",", 
         RowBox[{"reA4", "+", 
          RowBox[{"\[ImaginaryI]", " ", "imA4"}]}], ",", 
         RowBox[{"reA5", "+", 
          RowBox[{"\[ImaginaryI]", " ", "imA5"}]}], ",", 
         RowBox[{"reA6", "+", 
          RowBox[{"\[ImaginaryI]", " ", "imA6"}]}]}], "}"}], "//", 
       "FullSimplify"}]}], "]"}]}], ",", 
   RowBox[{"SaveDefinitions", "\[Rule]", "True"}]}], 
  "]"}], "\[IndentingNewLine]", 
 RowBox[{"Dynamic", "[", 
  RowBox[{
   RowBox[{"nulsol", " ", "=", " ", 
    RowBox[{"NullSpace", "[", 
     RowBox[{"{", 
      RowBox[{"M1", ",", "M2", ",", "M3", ",", "M4", ",", "M5", ",", "M6"}], 
      "}"}], "]"}]}], ",", 
   RowBox[{"SaveDefinitions", "\[Rule]", "True"}]}], 
  "]"}], "\[IndentingNewLine]", 
 RowBox[{"Dynamic", "[", 
  RowBox[{
   RowBox[{"Which", "[", 
    RowBox[{
     RowBox[{
      RowBox[{"Length", "[", "nulsol", "]"}], "\[Equal]", "0"}], ",", 
     "\[IndentingNewLine]", 
     RowBox[{
      RowBox[{"coef", "=", "\"\< \>\""}], ";", "\[IndentingNewLine]", 
      RowBox[{"plus", " ", "=", " ", "\"\< \>\""}], ";", 
      "\[IndentingNewLine]", 
      RowBox[{"with", " ", "=", " ", "\"\< \>\""}], ";", 
      "\[IndentingNewLine]", 
      RowBox[{"compl", " ", "=", " ", "\"\< \>\""}], ";", 
      "\[IndentingNewLine]", 
      RowBox[{"ToPrint", "=", "\"\< \>\""}]}], ",", "\[IndentingNewLine]", 
     RowBox[{
      RowBox[{"Length", "[", "nulsol", "]"}], "==", "1"}], ",", 
     "\[IndentingNewLine]", 
     RowBox[{
      RowBox[{"coef", "=", 
       RowBox[{"{", "\[Alpha]", "}"}]}], ";", "\[IndentingNewLine]", 
      RowBox[{"plus", " ", "=", " ", "\"\<+\>\""}], ";", 
      "\[IndentingNewLine]", 
      RowBox[{"with", " ", "=", " ", "\"\<With\>\""}], ";", 
      "\[IndentingNewLine]", 
      RowBox[{
      "compl", " ", "=", " ", "\"\<\[Element]\[DoubleStruckCapitalC]\>\""}], 
      ";", "\[IndentingNewLine]", 
      RowBox[{"ToPrint", " ", "=", " ", 
       RowBox[{
        RowBox[{"MatrixForm", "[", 
         RowBox[{"coef", "[", 
          RowBox[{"[", "1", "]"}], "]"}], "]"}], 
        RowBox[{"MatrixForm", "[", 
         RowBox[{"nulsol", "[", 
          RowBox[{"[", "1", "]"}], "]"}], "]"}]}]}]}], ",", 
     "\[IndentingNewLine]", 
     RowBox[{
      RowBox[{"Length", "[", "nulsol", "]"}], "==", "2"}], ",", 
     "\[IndentingNewLine]", 
     RowBox[{
      RowBox[{"coef", "=", 
       RowBox[{"{", 
        RowBox[{"\[Alpha]", ",", "\[Beta]"}], "}"}]}], ";", 
      "\[IndentingNewLine]", 
      RowBox[{"plus", " ", "=", " ", "\"\<+\>\""}], ";", 
      "\[IndentingNewLine]", 
      RowBox[{"with", " ", "=", " ", "\"\<With\>\""}], ";", 
      "\[IndentingNewLine]", 
      RowBox[{
      "compl", " ", "=", " ", "\"\<\[Element]\[DoubleStruckCapitalC]\>\""}], 
      ";", "\[IndentingNewLine]", 
      RowBox[{"ToPrint", " ", "=", " ", 
       RowBox[{
        RowBox[{
         RowBox[{"Dynamic", "[", 
          RowBox[{"MatrixForm", "[", 
           RowBox[{"coef", "[", 
            RowBox[{"[", "1", "]"}], "]"}], "]"}], "]"}], 
         RowBox[{"Dynamic", "[", 
          RowBox[{"MatrixForm", "[", 
           RowBox[{"nulsol", "[", 
            RowBox[{"[", "1", "]"}], "]"}], "]"}], "]"}]}], "+", 
        RowBox[{
         RowBox[{"Dynamic", "[", 
          RowBox[{"MatrixForm", "[", 
           RowBox[{"coef", "[", 
            RowBox[{"[", "2", "]"}], "]"}], "]"}], "]"}], 
         RowBox[{"Dynamic", "[", 
          RowBox[{"MatrixForm", "[", 
           RowBox[{"nulsol", "[", 
            RowBox[{"[", "2", "]"}], "]"}], "]"}], "]"}]}]}]}]}], ",", 
     "\[IndentingNewLine]", 
     RowBox[{
      RowBox[{"Length", "[", "nulsol", "]"}], "==", "3"}], ",", 
     "\[IndentingNewLine]", 
     RowBox[{
      RowBox[{"coef", "=", 
       RowBox[{"{", 
        RowBox[{"\[Alpha]", ",", "\[Beta]", ",", "\[Gamma]"}], "}"}]}], ";", 
      "\[IndentingNewLine]", 
      RowBox[{"plus", " ", "=", " ", "\"\<+\>\""}], ";", 
      "\[IndentingNewLine]", 
      RowBox[{"with", " ", "=", " ", "\"\<With\>\""}], ";", 
      "\[IndentingNewLine]", 
      RowBox[{
      "compl", " ", "=", " ", "\"\<\[Element]\[DoubleStruckCapitalC]\>\""}], 
      ";", "\[IndentingNewLine]", 
      RowBox[{"ToPrint", " ", "=", " ", 
       RowBox[{
        RowBox[{
         RowBox[{"Dynamic", "[", 
          RowBox[{"MatrixForm", "[", 
           RowBox[{"coef", "[", 
            RowBox[{"[", "1", "]"}], "]"}], "]"}], "]"}], 
         RowBox[{"Dynamic", "[", 
          RowBox[{"MatrixForm", "[", 
           RowBox[{"nulsol", "[", 
            RowBox[{"[", "1", "]"}], "]"}], "]"}], "]"}]}], "+", 
        RowBox[{
         RowBox[{"Dynamic", "[", 
          RowBox[{"MatrixForm", "[", 
           RowBox[{"coef", "[", 
            RowBox[{"[", "2", "]"}], "]"}], "]"}], "]"}], 
         RowBox[{"Dynamic", "[", 
          RowBox[{"MatrixForm", "[", 
           RowBox[{"nulsol", "[", 
            RowBox[{"[", "2", "]"}], "]"}], "]"}], "]"}]}], "+", 
        RowBox[{
         RowBox[{"Dynamic", "[", 
          RowBox[{"MatrixForm", "[", 
           RowBox[{"coef", "[", 
            RowBox[{"[", "3", "]"}], "]"}], "]"}], "]"}], 
         RowBox[{"Dynamic", "[", 
          RowBox[{"MatrixForm", "[", 
           RowBox[{"nulsol", "[", 
            RowBox[{"[", "3", "]"}], "]"}], "]"}], "]"}]}]}]}]}], ",", 
     "\[IndentingNewLine]", 
     RowBox[{
      RowBox[{"Length", "[", "nulsol", "]"}], "==", "4"}], ",", 
     "\[IndentingNewLine]", 
     RowBox[{
      RowBox[{"coef", "=", 
       RowBox[{"{", 
        RowBox[{
        "\[Alpha]", ",", "\[Beta]", ",", "\[Gamma]", ",", "\[Delta]"}], 
        "}"}]}], ";", "\[IndentingNewLine]", 
      RowBox[{"plus", " ", "=", " ", "\"\<+\>\""}], ";", 
      "\[IndentingNewLine]", 
      RowBox[{"with", " ", "=", " ", "\"\<With\>\""}], ";", 
      "\[IndentingNewLine]", 
      RowBox[{
      "compl", " ", "=", " ", "\"\<\[Element]\[DoubleStruckCapitalC]\>\""}], 
      ";", "\[IndentingNewLine]", 
      RowBox[{"ToPrint", " ", "=", " ", 
       RowBox[{
        RowBox[{
         RowBox[{"Dynamic", "[", 
          RowBox[{"MatrixForm", "[", 
           RowBox[{"coef", "[", 
            RowBox[{"[", "1", "]"}], "]"}], "]"}], "]"}], 
         RowBox[{"Dynamic", "[", 
          RowBox[{"MatrixForm", "[", 
           RowBox[{"nulsol", "[", 
            RowBox[{"[", "1", "]"}], "]"}], "]"}], "]"}]}], "+", 
        RowBox[{
         RowBox[{"Dynamic", "[", 
          RowBox[{"MatrixForm", "[", 
           RowBox[{"coef", "[", 
            RowBox[{"[", "2", "]"}], "]"}], "]"}], "]"}], 
         RowBox[{"Dynamic", "[", 
          RowBox[{"MatrixForm", "[", 
           RowBox[{"nulsol", "[", 
            RowBox[{"[", "2", "]"}], "]"}], "]"}], "]"}]}], "+", 
        RowBox[{
         RowBox[{"Dynamic", "[", 
          RowBox[{"MatrixForm", "[", 
           RowBox[{"coef", "[", 
            RowBox[{"[", "3", "]"}], "]"}], "]"}], "]"}], 
         RowBox[{"Dynamic", "[", 
          RowBox[{"MatrixForm", "[", 
           RowBox[{"nulsol", "[", 
            RowBox[{"[", "3", "]"}], "]"}], "]"}], "]"}]}], "+", 
        RowBox[{
         RowBox[{"Dynamic", "[", 
          RowBox[{"MatrixForm", "[", 
           RowBox[{"coef", "[", 
            RowBox[{"[", "4", "]"}], "]"}], "]"}], "]"}], 
         RowBox[{"Dynamic", "[", 
          RowBox[{"MatrixForm", "[", 
           RowBox[{"nulsol", "[", 
            RowBox[{"[", "4", "]"}], "]"}], "]"}], "]"}]}]}]}]}], ",", 
     "\[IndentingNewLine]", 
     RowBox[{
      RowBox[{"Length", "[", "nulsol", "]"}], "==", "5"}], ",", 
     "\[IndentingNewLine]", 
     RowBox[{
      RowBox[{"coef", "=", 
       RowBox[{"{", 
        RowBox[{
        "\[Alpha]", ",", "\[Beta]", ",", "\[Gamma]", ",", "\[Delta]", ",", 
         "\[Epsilon]"}], "}"}]}], ";", "\[IndentingNewLine]", 
      RowBox[{"plus", " ", "=", " ", "\"\<+\>\""}], ";", " ", 
      "\[IndentingNewLine]", 
      RowBox[{"with", " ", "=", " ", "\"\<With\>\""}], ";", 
      "\[IndentingNewLine]", 
      RowBox[{
      "compl", " ", "=", " ", "\"\<\[Element]\[DoubleStruckCapitalC]\>\""}], 
      ";", "\[IndentingNewLine]", 
      RowBox[{"ToPrint", " ", "=", " ", 
       RowBox[{
        RowBox[{
         RowBox[{"Dynamic", "[", 
          RowBox[{"MatrixForm", "[", 
           RowBox[{"coef", "[", 
            RowBox[{"[", "1", "]"}], "]"}], "]"}], "]"}], 
         RowBox[{"Dynamic", "[", 
          RowBox[{"MatrixForm", "[", 
           RowBox[{"nulsol", "[", 
            RowBox[{"[", "1", "]"}], "]"}], "]"}], "]"}]}], "+", 
        RowBox[{
         RowBox[{"Dynamic", "[", 
          RowBox[{"MatrixForm", "[", 
           RowBox[{"coef", "[", 
            RowBox[{"[", "2", "]"}], "]"}], "]"}], "]"}], 
         RowBox[{"Dynamic", "[", 
          RowBox[{"MatrixForm", "[", 
           RowBox[{"nulsol", "[", 
            RowBox[{"[", "2", "]"}], "]"}], "]"}], "]"}]}], "+", 
        RowBox[{
         RowBox[{"Dynamic", "[", 
          RowBox[{"MatrixForm", "[", 
           RowBox[{"coef", "[", 
            RowBox[{"[", "3", "]"}], "]"}], "]"}], "]"}], 
         RowBox[{"Dynamic", "[", 
          RowBox[{"MatrixForm", "[", 
           RowBox[{"nulsol", "[", 
            RowBox[{"[", "3", "]"}], "]"}], "]"}], "]"}]}], "+", 
        RowBox[{
         RowBox[{"Dynamic", "[", 
          RowBox[{"MatrixForm", "[", 
           RowBox[{"coef", "[", 
            RowBox[{"[", "4", "]"}], "]"}], "]"}], "]"}], 
         RowBox[{"Dynamic", "[", 
          RowBox[{"MatrixForm", "[", 
           RowBox[{"nulsol", "[", 
            RowBox[{"[", "4", "]"}], "]"}], "]"}], "]"}]}], "+", 
        RowBox[{
         RowBox[{"Dynamic", "[", 
          RowBox[{"MatrixForm", "[", 
           RowBox[{"coef", "[", 
            RowBox[{"[", "5", "]"}], "]"}], "]"}], "]"}], 
         RowBox[{"Dynamic", "[", 
          RowBox[{"MatrixForm", "[", 
           RowBox[{"nulsol", "[", 
            RowBox[{"[", "5", "]"}], "]"}], "]"}], "]"}]}]}]}]}]}], "]"}], 
   ",", 
   RowBox[{"SaveDefinitions", "\[Rule]", "True"}]}], 
  "]"}], "\[IndentingNewLine]", 
 RowBox[{
  RowBox[{"pol", "=", 
   RowBox[{"{", 
    RowBox[{
     RowBox[{"\"\<spol\>\"", "->", "\"\<s\>\""}], ",", 
     RowBox[{"\"\<ppol\>\"", "->", "\"\<p\>\""}]}], "}"}]}], 
  ";"}], "\[IndentingNewLine]", 
 RowBox[{
  RowBox[{
   RowBox[{"plane", "=", 
    RowBox[{"{", 
     RowBox[{
      RowBox[{"\"\<zplus\>\"", "->", "\"\<z>0\>\""}], ",", 
      RowBox[{"\"\<zmin\>\"", "->", "\"\<z<0\>\""}]}], "}"}]}], ";"}], 
  "\[IndentingNewLine]"}], "\[IndentingNewLine]", 
 RowBox[{"Framed", "[", 
  RowBox[{"Style", "[", 
   RowBox[{
   "\"\<Insert here the normalised wavevectors for each of the points you \
want to fix in the electric field spectra, the polarisation and half-space \
for each of the points and the complex amplitudes of the spectra at said \
points.\\nThe code returns the values for the 6 dipole moments components: \!\
\(\*SubscriptBox[\(p\), \(x\)]\), \!\(\*SubscriptBox[\(p\), \(y\)]\), \
\!\(\*SubscriptBox[\(p\), \(z\)]\), \!\(\*SubscriptBox[\(m\), \(x\)]\), \
\!\(\*SubscriptBox[\(m\), \(y\)]\), \!\(\*SubscriptBox[\(m\), \(z\)]\) as a \
particular solution + the associated homogeneous system's solutions, each \
multiplied by an arbitrary complex coefficient.\\nThe code is solving the \
linear system for the electric field angular spectrum with a value of \
\!\(\*SubscriptBox[\(k\), \(0\)]\)=\!\(\*FractionBox[\(2  \[Pi]\), \
\(\[Lambda]\)]\)=1, so the dipoles are normalised by a factor of \
\!\(\*SubscriptBox[\(k\), \(0\)]\)\>\"", ",", 
    RowBox[{"FontFamily", "\[Rule]", "\"\<Helvetica\>\""}], ",", "Blue"}], 
   "]"}], "]"}], "\[IndentingNewLine]", 
 RowBox[{"Grid", "[", 
  RowBox[{
   RowBox[{"{", 
    RowBox[{
     RowBox[{"{", 
      RowBox[{"\"\< \>\"", ",", "\"\< \>\"", ",", " ", 
       RowBox[{"Style", "[", 
        RowBox[{
        "\"\<Normalised \!\(\*SubscriptBox[\(k\), \(x\)]\) \
(\!\(\*SubscriptBox[\(k\), \(x\)]\)/\!\(\*SubscriptBox[\(k\), \(0\)]\))\>\"", 
         ",", 
         RowBox[{"FontWeight", "\[Rule]", "Bold"}], ",", "Larger", ",", 
         RowBox[{"FontFamily", "\[Rule]", "\"\<Helvetica\>\""}]}], "]"}], ",",
        " ", 
       RowBox[{"Style", "[", 
        RowBox[{
        "\"\<Normalised (\!\(\*SubscriptBox[\(k\), \
\(y\)]\)/\!\(\*SubscriptBox[\(k\), \(0\)]\))\>\"", ",", 
         RowBox[{"FontWeight", "\[Rule]", "Bold"}], ",", "Larger", ",", 
         RowBox[{"FontFamily", "\[Rule]", "\"\<Helvetica\>\""}]}], "]"}], ",",
        " ", 
       RowBox[{"Style", "[", 
        RowBox[{"\"\<Select polarisation\>\"", ",", 
         RowBox[{"FontWeight", "\[Rule]", "Bold"}], ",", "Larger", ",", 
         RowBox[{"FontFamily", "\[Rule]", "\"\<Helvetica\>\""}]}], "]"}], ",",
        " ", 
       RowBox[{"Style", "[", 
        RowBox[{"\"\<Select half-space\>\"", ",", 
         RowBox[{"FontWeight", "\[Rule]", "Bold"}], ",", "Larger", ",", 
         RowBox[{"FontFamily", "\[Rule]", "\"\<Helvetica\>\""}]}], "]"}], ",", 
       RowBox[{"Style", "[", 
        RowBox[{"\"\<Complex amplitude (A+iB)\>\"", ",", 
         RowBox[{"FontWeight", "\[Rule]", "Bold"}], ",", "Larger", ",", 
         RowBox[{"FontFamily", "\[Rule]", "\"\<Helvetica\>\""}]}], "]"}], ",",
        "SpanFromLeft", ",", "SpanFromLeft"}], "}"}], ",", 
     "\[IndentingNewLine]", 
     RowBox[{"{", 
      RowBox[{
       RowBox[{"Checkbox", "[", 
        RowBox[{
         RowBox[{"Dynamic", "[", "point1", "]"}], ",", 
         RowBox[{"{", 
          RowBox[{"True", ",", "False"}], "}"}]}], "]"}], ",", 
       RowBox[{"Style", "[", 
        RowBox[{"\"\<1st point\>\"", ",", 
         RowBox[{"FontFamily", "\[Rule]", "\"\<Helvetica\>\""}]}], "]"}], ",", 
       RowBox[{"InputField", "[", 
        RowBox[{
         RowBox[{"Dynamic", "[", "kx1", "]"}], ",", "Number", ",", 
         RowBox[{"FieldSize", "\[Rule]", "Tiny"}], ",", " ", 
         RowBox[{"FieldHint", "\[Rule]", "\"\<kx1/k0\>\""}]}], "]"}], ",", 
       RowBox[{"InputField", "[", 
        RowBox[{
         RowBox[{"Dynamic", "[", "ky1", "]"}], ",", "Number", ",", 
         RowBox[{"FieldSize", "\[Rule]", "Tiny"}], ",", 
         RowBox[{"FieldHint", "\[Rule]", "\"\<ky1/k0\>\""}]}], "]"}], ",", 
       RowBox[{"RadioButtonBar", "[", 
        RowBox[{
         RowBox[{"Dynamic", "[", "pol1", "]"}], ",", "pol"}], "]"}], ",", 
       RowBox[{"RadioButtonBar", "[", 
        RowBox[{
         RowBox[{"Dynamic", "[", "plane1", "]"}], ",", "plane"}], "]"}], ",", 
       RowBox[{"InputField", "[", 
        RowBox[{
         RowBox[{"Dynamic", "[", 
          RowBox[{"reA1", ",", 
           RowBox[{"SaveDefinitions", "\[Rule]", "True"}]}], "]"}], ",", 
         "Number", ",", 
         RowBox[{"FieldSize", "\[Rule]", "Tiny"}], ",", 
         RowBox[{"FieldHint", "\[Rule]", "\"\<Re[A1]\>\""}]}], "]"}], ",", 
       "\"\<+\[ImaginaryI]\>\"", ",", 
       RowBox[{"InputField", "[", 
        RowBox[{
         RowBox[{"Dynamic", "[", 
          RowBox[{"imA1", ",", 
           RowBox[{"SaveDefinitions", "\[Rule]", "True"}]}], "]"}], ",", 
         "Number", ",", 
         RowBox[{"FieldSize", "\[Rule]", "Tiny"}], ",", 
         RowBox[{"FieldHint", "\[Rule]", "\"\<Im[A1]\>\""}]}], "]"}]}], "}"}],
      ",", "\[IndentingNewLine]", 
     RowBox[{"{", 
      RowBox[{
       RowBox[{"Checkbox", "[", 
        RowBox[{
         RowBox[{"Dynamic", "[", "point2", "]"}], ",", 
         RowBox[{"{", 
          RowBox[{"True", ",", "False"}], "}"}]}], "]"}], ",", 
       RowBox[{"Style", "[", 
        RowBox[{"\"\<2nd point\>\"", ",", 
         RowBox[{"FontFamily", "\[Rule]", "\"\<Helvetica\>\""}]}], "]"}], ",", 
       RowBox[{"InputField", "[", 
        RowBox[{
         RowBox[{"Dynamic", "[", "kx2", "]"}], ",", "Number", ",", 
         RowBox[{"FieldSize", "\[Rule]", "Tiny"}], ",", 
         RowBox[{"FieldHint", "\[Rule]", "\"\<kx2/k0\>\""}]}], "]"}], ",", 
       RowBox[{"InputField", "[", 
        RowBox[{
         RowBox[{"Dynamic", "[", "ky2", "]"}], ",", "Number", ",", 
         RowBox[{"FieldSize", "\[Rule]", "Tiny"}], ",", 
         RowBox[{"FieldHint", "\[Rule]", "\"\<ky2/k0\>\""}]}], "]"}], ",", 
       RowBox[{"RadioButtonBar", "[", 
        RowBox[{
         RowBox[{"Dynamic", "[", "pol2", "]"}], ",", "pol"}], "]"}], ",", 
       RowBox[{"RadioButtonBar", "[", 
        RowBox[{
         RowBox[{"Dynamic", "[", "plane2", "]"}], ",", "plane"}], "]"}], ",", 
       RowBox[{"InputField", "[", 
        RowBox[{
         RowBox[{"Dynamic", "[", 
          RowBox[{"reA2", ",", 
           RowBox[{"SaveDefinitions", "\[Rule]", "True"}]}], "]"}], ",", 
         "Number", ",", 
         RowBox[{"FieldSize", "\[Rule]", "Tiny"}], ",", 
         RowBox[{"FieldHint", "\[Rule]", "\"\<Re[A2]\>\""}]}], "]"}], ",", 
       "\"\<+\[ImaginaryI]\>\"", ",", 
       RowBox[{"InputField", "[", 
        RowBox[{
         RowBox[{"Dynamic", "[", 
          RowBox[{"imA2", ",", 
           RowBox[{"SaveDefinitions", "\[Rule]", "True"}]}], "]"}], ",", 
         "Number", ",", 
         RowBox[{"FieldSize", "\[Rule]", "Tiny"}], ",", 
         RowBox[{"FieldHint", "\[Rule]", "\"\<Im[A2]\>\""}]}], "]"}]}], "}"}],
      ",", "\[IndentingNewLine]", 
     RowBox[{"{", 
      RowBox[{
       RowBox[{"Checkbox", "[", 
        RowBox[{
         RowBox[{"Dynamic", "[", "point3", "]"}], ",", 
         RowBox[{"{", 
          RowBox[{"True", ",", "False"}], "}"}]}], "]"}], ",", 
       RowBox[{"Style", "[", 
        RowBox[{"\"\<3rd point\>\"", ",", 
         RowBox[{"FontFamily", "\[Rule]", "\"\<Helvetica\>\""}]}], "]"}], ",", 
       RowBox[{"InputField", "[", 
        RowBox[{
         RowBox[{"Dynamic", "[", "kx3", "]"}], ",", "Number", ",", 
         RowBox[{"FieldSize", "\[Rule]", "Tiny"}], ",", 
         RowBox[{"FieldHint", "\[Rule]", "\"\<kx3/k0\>\""}]}], "]"}], ",", 
       RowBox[{"InputField", "[", 
        RowBox[{
         RowBox[{"Dynamic", "[", "ky3", "]"}], ",", "Number", ",", 
         RowBox[{"FieldSize", "\[Rule]", "Tiny"}], ",", 
         RowBox[{"FieldHint", "\[Rule]", "\"\<ky3/k0\>\""}]}], "]"}], ",", 
       RowBox[{"RadioButtonBar", "[", 
        RowBox[{
         RowBox[{"Dynamic", "[", "pol3", "]"}], ",", "pol"}], "]"}], ",", 
       RowBox[{"RadioButtonBar", "[", 
        RowBox[{
         RowBox[{"Dynamic", "[", "plane3", "]"}], ",", "plane"}], "]"}], ",", 
       RowBox[{"InputField", "[", 
        RowBox[{
         RowBox[{"Dynamic", "[", 
          RowBox[{"reA3", ",", 
           RowBox[{"SaveDefinitions", "\[Rule]", "True"}]}], "]"}], ",", 
         "Number", ",", 
         RowBox[{"FieldSize", "\[Rule]", "Tiny"}], ",", 
         RowBox[{"FieldHint", "\[Rule]", "\"\<Re[A3]\>\""}]}], "]"}], ",", 
       "\"\<+\[ImaginaryI]\>\"", ",", 
       RowBox[{"InputField", "[", 
        RowBox[{
         RowBox[{"Dynamic", "[", 
          RowBox[{"imA3", ",", 
           RowBox[{"SaveDefinitions", "\[Rule]", "True"}]}], "]"}], ",", 
         "Number", ",", 
         RowBox[{"FieldSize", "\[Rule]", "Tiny"}], ",", 
         RowBox[{"FieldHint", "\[Rule]", "\"\<Im[A3]\>\""}]}], "]"}]}], "}"}],
      ",", "\[IndentingNewLine]", 
     RowBox[{"{", 
      RowBox[{
       RowBox[{"Checkbox", "[", 
        RowBox[{
         RowBox[{"Dynamic", "[", "point4", "]"}], ",", 
         RowBox[{"{", 
          RowBox[{"True", ",", "False"}], "}"}]}], "]"}], ",", 
       RowBox[{"Style", "[", 
        RowBox[{"\"\<4th point\>\"", ",", 
         RowBox[{"FontFamily", "\[Rule]", "\"\<Helvetica\>\""}]}], "]"}], ",", 
       RowBox[{"InputField", "[", 
        RowBox[{
         RowBox[{"Dynamic", "[", "kx4", "]"}], ",", "Number", ",", 
         RowBox[{"FieldSize", "\[Rule]", "Tiny"}], ",", 
         RowBox[{"FieldHint", "\[Rule]", "\"\<kx4/k0\>\""}]}], "]"}], ",", 
       RowBox[{"InputField", "[", 
        RowBox[{
         RowBox[{"Dynamic", "[", "ky4", "]"}], ",", "Number", ",", 
         RowBox[{"FieldSize", "\[Rule]", "Tiny"}], ",", 
         RowBox[{"FieldHint", "\[Rule]", "\"\<ky4/k0\>\""}]}], "]"}], ",", 
       RowBox[{"RadioButtonBar", "[", 
        RowBox[{
         RowBox[{"Dynamic", "[", "pol4", "]"}], ",", "pol"}], "]"}], ",", 
       RowBox[{"RadioButtonBar", "[", 
        RowBox[{
         RowBox[{"Dynamic", "[", "plane4", "]"}], ",", "plane"}], "]"}], ",", 
       RowBox[{"InputField", "[", 
        RowBox[{
         RowBox[{"Dynamic", "[", 
          RowBox[{"reA4", ",", 
           RowBox[{"SaveDefinitions", "\[Rule]", "True"}]}], "]"}], ",", 
         "Number", ",", 
         RowBox[{"FieldSize", "\[Rule]", "Tiny"}], ",", 
         RowBox[{"FieldHint", "\[Rule]", "\"\<Re[A4]\>\""}]}], "]"}], ",", 
       "\"\<+\[ImaginaryI]\>\"", ",", 
       RowBox[{"InputField", "[", 
        RowBox[{
         RowBox[{"Dynamic", "[", 
          RowBox[{"imA4", ",", 
           RowBox[{"SaveDefinitions", "\[Rule]", "True"}]}], "]"}], ",", 
         "Number", ",", 
         RowBox[{"FieldSize", "\[Rule]", "Tiny"}], ",", 
         RowBox[{"FieldHint", "\[Rule]", "\"\<Im[A4]\>\""}]}], "]"}]}], "}"}],
      ",", "\[IndentingNewLine]", 
     RowBox[{"{", 
      RowBox[{
       RowBox[{"Checkbox", "[", 
        RowBox[{
         RowBox[{"Dynamic", "[", "point5", "]"}], ",", 
         RowBox[{"{", 
          RowBox[{"True", ",", "False"}], "}"}]}], "]"}], ",", 
       RowBox[{"Style", "[", 
        RowBox[{"\"\<5th point\>\"", ",", 
         RowBox[{"FontFamily", "\[Rule]", "\"\<Helvetica\>\""}]}], "]"}], ",", 
       RowBox[{"InputField", "[", 
        RowBox[{
         RowBox[{"Dynamic", "[", "kx5", "]"}], ",", "Number", ",", 
         RowBox[{"FieldSize", "\[Rule]", "Tiny"}], ",", 
         RowBox[{"FieldHint", "\[Rule]", "\"\<kx5/k0\>\""}]}], "]"}], ",", 
       RowBox[{"InputField", "[", 
        RowBox[{
         RowBox[{"Dynamic", "[", "ky5", "]"}], ",", "Number", ",", 
         RowBox[{"FieldSize", "\[Rule]", "Tiny"}], ",", 
         RowBox[{"FieldHint", "\[Rule]", "\"\<ky5/k0\>\""}]}], "]"}], ",", 
       RowBox[{"RadioButtonBar", "[", 
        RowBox[{
         RowBox[{"Dynamic", "[", "pol5", "]"}], ",", "pol"}], "]"}], ",", 
       RowBox[{"RadioButtonBar", "[", 
        RowBox[{
         RowBox[{"Dynamic", "[", "plane5", "]"}], ",", "plane"}], "]"}], ",", 
       RowBox[{"InputField", "[", 
        RowBox[{
         RowBox[{"Dynamic", "[", 
          RowBox[{"reA5", ",", 
           RowBox[{"SaveDefinitions", "\[Rule]", "True"}]}], "]"}], ",", 
         "Number", ",", 
         RowBox[{"FieldSize", "\[Rule]", "Tiny"}], ",", 
         RowBox[{"FieldHint", "\[Rule]", "\"\<Re[A5]\>\""}]}], "]"}], ",", 
       "\"\<+\[ImaginaryI]\>\"", ",", 
       RowBox[{"InputField", "[", 
        RowBox[{
         RowBox[{"Dynamic", "[", 
          RowBox[{"imA5", ",", 
           RowBox[{"SaveDefinitions", "\[Rule]", "True"}]}], "]"}], ",", 
         "Number", ",", 
         RowBox[{"FieldSize", "\[Rule]", "Tiny"}], ",", 
         RowBox[{"FieldHint", "\[Rule]", "\"\<Im[A5]\>\""}]}], "]"}]}], "}"}],
      ",", "\[IndentingNewLine]", 
     RowBox[{"{", 
      RowBox[{
       RowBox[{"Checkbox", "[", 
        RowBox[{
         RowBox[{"Dynamic", "[", "point6", "]"}], ",", 
         RowBox[{"{", 
          RowBox[{"True", ",", "False"}], "}"}]}], "]"}], ",", 
       RowBox[{"Style", "[", 
        RowBox[{"\"\<6th point\>\"", ",", 
         RowBox[{"FontFamily", "\[Rule]", "\"\<Helvetica\>\""}]}], "]"}], ",", 
       RowBox[{"InputField", "[", 
        RowBox[{
         RowBox[{"Dynamic", "[", "kx6", "]"}], ",", "Number", ",", 
         RowBox[{"FieldSize", "\[Rule]", "Tiny"}], ",", 
         RowBox[{"FieldHint", "\[Rule]", "\"\<kx6/k0\>\""}]}], "]"}], ",", 
       RowBox[{"InputField", "[", 
        RowBox[{
         RowBox[{"Dynamic", "[", "ky6", "]"}], ",", "Number", ",", 
         RowBox[{"FieldSize", "\[Rule]", "Tiny"}], ",", 
         RowBox[{"FieldHint", "\[Rule]", "\"\<ky6/k0\>\""}]}], "]"}], ",", 
       RowBox[{"RadioButtonBar", "[", 
        RowBox[{
         RowBox[{"Dynamic", "[", "pol6", "]"}], ",", "pol"}], "]"}], ",", 
       RowBox[{"RadioButtonBar", "[", 
        RowBox[{
         RowBox[{"Dynamic", "[", "plane6", "]"}], ",", "plane"}], "]"}], ",", 
       RowBox[{"InputField", "[", 
        RowBox[{
         RowBox[{"Dynamic", "[", 
          RowBox[{"reA6", ",", 
           RowBox[{"SaveDefinitions", "\[Rule]", "True"}]}], "]"}], ",", 
         "Number", ",", 
         RowBox[{"FieldSize", "\[Rule]", "Tiny"}], ",", 
         RowBox[{"FieldHint", "\[Rule]", "\"\<Re[A6]\>\""}]}], "]"}], ",", 
       "\"\<+\[ImaginaryI]\>\"", ",", 
       RowBox[{"InputField", "[", 
        RowBox[{
         RowBox[{"Dynamic", "[", 
          RowBox[{"imA6", ",", 
           RowBox[{"SaveDefinitions", "\[Rule]", "True"}]}], "]"}], ",", 
         "Number", ",", 
         RowBox[{"FieldSize", "\[Rule]", "Tiny"}], ",", 
         RowBox[{"FieldHint", "\[Rule]", "\"\<Im[A6]\>\""}]}], "]"}]}], 
      "}"}]}], "}"}], "\[IndentingNewLine]", ",", 
   RowBox[{"Frame", "\[Rule]", "All"}]}], "]"}], "\[IndentingNewLine]", 
 RowBox[{"Print", "[", 
  RowBox[{
   RowBox[{"Style", "[", 
    RowBox[{
     SubscriptBox["k", "0"], ",", 
     RowBox[{"FontFamily", "\[Rule]", "\"\<Helvetica\>\""}], ",", 
     RowBox[{"FontSize", "\[Rule]", "14"}]}], "]"}], ",", 
   RowBox[{"Style", "[", 
    RowBox[{
     RowBox[{"MatrixForm", "[", 
      RowBox[{"{", 
       RowBox[{
       "\"\<\!\(\*SubscriptBox[\(p\), \(x\)]\)\>\"", ",", 
        "\"\<\!\(\*SubscriptBox[\(p\), \(y\)]\)\>\"", ",", 
        "\"\<\!\(\*SubscriptBox[\(p\), \(z\)]\)\>\"", ",", 
        "\"\<\!\(\*SubscriptBox[\(m\), \(x\)]\)\>\"", ",", 
        "\"\<\!\(\*SubscriptBox[\(m\), \(y\)]\)\>\"", ",", 
        "\"\<\!\(\*SubscriptBox[\(m\), \(z\)]\)\>\""}], "}"}], "]"}], ",", 
     RowBox[{"FontFamily", "\[Rule]", "\"\<Helvetica\>\""}], ",", 
     RowBox[{"FontSize", "\[Rule]", "14"}]}], "]"}], ",", 
   RowBox[{"Style", "[", 
    RowBox[{"\"\<=\>\"", ",", 
     RowBox[{"FontFamily", "\[Rule]", "\"\<Helvetica\>\""}], ",", 
     RowBox[{"FontSize", "\[Rule]", "14"}]}], "]"}], ",", " ", 
   RowBox[{"Style", "[", 
    RowBox[{
     RowBox[{"Dynamic", "[", 
      RowBox[{
       RowBox[{
        RowBox[{"sol", "//", "Simplify"}], "//", "MatrixForm"}], ",", 
       RowBox[{"SaveDefinitions", "\[Rule]", "True"}]}], "]"}], ",", 
     RowBox[{"FontFamily", "\[Rule]", "\"\<Helvetica\>\""}], ",", 
     RowBox[{"FontSize", "\[Rule]", "14"}]}], "]"}], ",", 
   RowBox[{"Style", "[", 
    RowBox[{
     RowBox[{"Dynamic", "[", 
      RowBox[{"plus", ",", 
       RowBox[{"SaveDefinitions", "\[Rule]", "True"}]}], "]"}], ",", 
     RowBox[{"FontFamily", "\[Rule]", "\"\<Helvetica\>\""}], ",", 
     RowBox[{"FontSize", "\[Rule]", "14"}]}], "]"}], ",", 
   RowBox[{"Style", "[", 
    RowBox[{
     RowBox[{"Dynamic", "[", 
      RowBox[{
       RowBox[{"ToPrint", "//", "Simplify"}], ",", 
       RowBox[{"SaveDefinitions", "\[Rule]", "True"}]}], "]"}], ",", 
     RowBox[{"FontFamily", "\[Rule]", "\"\<Helvetica\>\""}], ",", 
     RowBox[{"FontSize", "\[Rule]", "14"}]}], "]"}], ",", 
   RowBox[{"Style", "[", 
    RowBox[{"\"\< \>\"", ",", 
     RowBox[{"FontFamily", "\[Rule]", "\"\<Helvetica\>\""}], ",", 
     RowBox[{"FontSize", "\[Rule]", "64"}]}], "]"}], ",", 
   RowBox[{"Style", "[", 
    RowBox[{
     RowBox[{"Dynamic", "[", 
      RowBox[{"with", ",", 
       RowBox[{"SaveDefinitions", "\[Rule]", "True"}]}], "]"}], ",", 
     RowBox[{"FontFamily", "\[Rule]", "\"\<Helvetica\>\""}], ",", 
     RowBox[{"FontSize", "\[Rule]", "14"}]}], "]"}], ",", 
   RowBox[{"Style", "[", 
    RowBox[{"\"\< \>\"", ",", 
     RowBox[{"FontFamily", "\[Rule]", "\"\<Helvetica\>\""}], ",", 
     RowBox[{"FontSize", "\[Rule]", "14"}]}], "]"}], ",", 
   RowBox[{"Style", "[", 
    RowBox[{
     RowBox[{"Dynamic", "[", 
      RowBox[{"coef", ",", 
       RowBox[{"SaveDefinitions", "\[Rule]", "True"}]}], "]"}], ",", 
     RowBox[{"FontFamily", "\[Rule]", "\"\<Helvetica\>\""}], ",", 
     RowBox[{"FontSize", "\[Rule]", "14"}]}], "]"}], ",", 
   RowBox[{"Style", "[", 
    RowBox[{"\"\< \>\"", ",", 
     RowBox[{"FontFamily", "\[Rule]", "\"\<Helvetica\>\""}], ",", 
     RowBox[{"FontSize", "\[Rule]", "14"}]}], "]"}], ",", 
   RowBox[{"Style", "[", 
    RowBox[{
     RowBox[{"Dynamic", "[", 
      RowBox[{"compl", ",", 
       RowBox[{"SaveDefinitions", "\[Rule]", "True"}]}], "]"}], ",", 
     RowBox[{"FontFamily", "\[Rule]", "\"\<Helvetica\>\""}], ",", 
     RowBox[{"FontSize", "\[Rule]", "14"}]}], "]"}]}], 
  "]"}], "\[IndentingNewLine]"}], "Input",
 Editable->False,
 CellOpen->False,
 ExpressionUUID -> "60847027-2c3e-465f-99c2-d5fe2190a4df"],

Cell[BoxData[
 DynamicBox[ToBoxes[
   If[$CellContext`point1 == False, 
    Dynamic[
     Which[
      And[$CellContext`pol1 == "spol", $CellContext`plane1 == 
       "zplus"], $CellContext`M1 = Coefficient[
        ReplaceAll[$CellContext`EsEpp, {$CellContext`kx -> $CellContext`kx1, \
$CellContext`ky -> $CellContext`ky1}], {$CellContext`px, $CellContext`py, \
$CellContext`pz, $CellContext`mx, $CellContext`my, $CellContext`mz}], 
      And[$CellContext`pol1 == "spol", $CellContext`plane1 == 
       "zmin"], $CellContext`M1 = Coefficient[
        ReplaceAll[$CellContext`EsEpm, {$CellContext`kx -> $CellContext`kx1, \
$CellContext`ky -> $CellContext`ky1}], {$CellContext`px, $CellContext`py, \
$CellContext`pz, $CellContext`mx, $CellContext`my, $CellContext`mz}], 
      And[$CellContext`pol1 == "ppol", $CellContext`plane1 == 
       "zplus"], $CellContext`M1 = Coefficient[
        ReplaceAll[$CellContext`EpEpp, {$CellContext`kx -> $CellContext`kx1, \
$CellContext`ky -> $CellContext`ky1}], {$CellContext`px, $CellContext`py, \
$CellContext`pz, $CellContext`mx, $CellContext`my, $CellContext`mz}], 
      And[$CellContext`pol1 == "ppol", $CellContext`plane1 == 
       "zmin"], $CellContext`M1 = Coefficient[
        ReplaceAll[$CellContext`EpEpm, {$CellContext`kx -> $CellContext`kx1, \
$CellContext`ky -> $CellContext`ky1}], {$CellContext`px, $CellContext`py, \
$CellContext`pz, $CellContext`mx, $CellContext`my, $CellContext`mz}]], 
     SaveDefinitions -> 
     True], $CellContext`M1 = {0, 0, 0, 0, 0, 0}; $CellContext`reA1 = 
     0; $CellContext`imA1 = 0], StandardForm],
  ImageSizeCache->{997.4, {22., 60.}},
  Initialization:>{$CellContext`EsEpp = (((I/
        8) $CellContext`k1^2) (-(((($CellContext`kx $CellContext`kz1) \
$CellContext`mx)/($CellContext`k1 $CellContext`kt) + (($CellContext`ky \
$CellContext`kz1) $CellContext`my)/($CellContext`k1 $CellContext`kt) + \
((-$CellContext`kx^2 - $CellContext`ky^2) $CellContext`mz)/($CellContext`k1 \
$CellContext`kt))/$CellContext`c) - ($CellContext`ky \
$CellContext`px)/$CellContext`kt + ($CellContext`kx \
$CellContext`py)/$CellContext`kt))/($CellContext`kz1 Pi^2), $CellContext`k1 = 
    1, $CellContext`kz1 = 
    Sqrt[$CellContext`k1^2 - $CellContext`kt^2], $CellContext`kt = 
    Sqrt[$CellContext`kx^2 + $CellContext`ky^2], $CellContext`EsEpm = (((I/
        8) $CellContext`k1^2) (-((-((($CellContext`kx $CellContext`kz1) \
$CellContext`mx)/($CellContext`k1 $CellContext`kt)) - (($CellContext`ky \
$CellContext`kz1) $CellContext`my)/($CellContext`k1 $CellContext`kt) + \
((-$CellContext`kx^2 - $CellContext`ky^2) $CellContext`mz)/($CellContext`k1 \
$CellContext`kt))/$CellContext`c) - ($CellContext`ky \
$CellContext`px)/$CellContext`kt + ($CellContext`kx \
$CellContext`py)/$CellContext`kt))/($CellContext`kz1 
     Pi^2), $CellContext`EpEpp = (((I/
        8) $CellContext`k1^2) ((-(($CellContext`ky \
$CellContext`mx)/$CellContext`kt) + ($CellContext`kx \
$CellContext`my)/$CellContext`kt)/$CellContext`c + (($CellContext`kx \
$CellContext`kz1) $CellContext`px)/($CellContext`k1 $CellContext`kt) + \
(($CellContext`ky $CellContext`kz1) $CellContext`py)/($CellContext`k1 \
$CellContext`kt) + ((-$CellContext`kx^2 - $CellContext`ky^2) \
$CellContext`pz)/($CellContext`k1 $CellContext`kt)))/($CellContext`kz1 
     Pi^2), $CellContext`EpEpm = (((I/
        8) $CellContext`k1^2) ((-(($CellContext`ky \
$CellContext`mx)/$CellContext`kt) + ($CellContext`kx \
$CellContext`my)/$CellContext`kt)/$CellContext`c - (($CellContext`kx \
$CellContext`kz1) $CellContext`px)/($CellContext`k1 $CellContext`kt) - \
(($CellContext`ky $CellContext`kz1) $CellContext`py)/($CellContext`k1 \
$CellContext`kt) + ((-$CellContext`kx^2 - $CellContext`ky^2) \
$CellContext`pz)/($CellContext`k1 $CellContext`kt)))/($CellContext`kz1 
     Pi^2)}]], "Output",
 Editable->False,
 CellOpen->False],

Cell[BoxData[
 DynamicBox[ToBoxes[
   If[$CellContext`point2 == False, 
    Dynamic[
     Which[
      And[$CellContext`pol2 == "spol", $CellContext`plane2 == 
       "zplus"], $CellContext`M2 = Coefficient[
        ReplaceAll[$CellContext`EsEpp, {$CellContext`kx -> $CellContext`kx2, \
$CellContext`ky -> $CellContext`ky2}], {$CellContext`px, $CellContext`py, \
$CellContext`pz, $CellContext`mx, $CellContext`my, $CellContext`mz}], 
      And[$CellContext`pol2 == "spol", $CellContext`plane2 == 
       "zmin"], $CellContext`M2 = Coefficient[
        ReplaceAll[$CellContext`EsEpm, {$CellContext`kx -> $CellContext`kx2, \
$CellContext`ky -> $CellContext`ky2}], {$CellContext`px, $CellContext`py, \
$CellContext`pz, $CellContext`mx, $CellContext`my, $CellContext`mz}], 
      And[$CellContext`pol2 == "ppol", $CellContext`plane2 == 
       "zplus"], $CellContext`M2 = Coefficient[
        ReplaceAll[$CellContext`EpEpp, {$CellContext`kx -> $CellContext`kx2, \
$CellContext`ky -> $CellContext`ky2}], {$CellContext`px, $CellContext`py, \
$CellContext`pz, $CellContext`mx, $CellContext`my, $CellContext`mz}], 
      And[$CellContext`pol2 == "ppol", $CellContext`plane2 == 
       "zmin"], $CellContext`M2 = Coefficient[
        ReplaceAll[$CellContext`EpEpm, {$CellContext`kx -> $CellContext`kx2, \
$CellContext`ky -> $CellContext`ky2}], {$CellContext`px, $CellContext`py, \
$CellContext`pz, $CellContext`mx, $CellContext`my, $CellContext`mz}]], 
     SaveDefinitions -> 
     True], $CellContext`M2 = {0, 0, 0, 0, 0, 0}; $CellContext`reA2 = 
     0; $CellContext`imA2 = 0], StandardForm],
  ImageSizeCache->{7., {0., 8.}},
  Initialization:>{$CellContext`EsEpp = (((I/
        8) $CellContext`k1^2) (-(((($CellContext`kx $CellContext`kz1) \
$CellContext`mx)/($CellContext`k1 $CellContext`kt) + (($CellContext`ky \
$CellContext`kz1) $CellContext`my)/($CellContext`k1 $CellContext`kt) + \
((-$CellContext`kx^2 - $CellContext`ky^2) $CellContext`mz)/($CellContext`k1 \
$CellContext`kt))/$CellContext`c) - ($CellContext`ky \
$CellContext`px)/$CellContext`kt + ($CellContext`kx \
$CellContext`py)/$CellContext`kt))/($CellContext`kz1 Pi^2), $CellContext`k1 = 
    1, $CellContext`kz1 = 
    Sqrt[$CellContext`k1^2 - $CellContext`kt^2], $CellContext`kt = 
    Sqrt[$CellContext`kx^2 + $CellContext`ky^2], $CellContext`EsEpm = (((I/
        8) $CellContext`k1^2) (-((-((($CellContext`kx $CellContext`kz1) \
$CellContext`mx)/($CellContext`k1 $CellContext`kt)) - (($CellContext`ky \
$CellContext`kz1) $CellContext`my)/($CellContext`k1 $CellContext`kt) + \
((-$CellContext`kx^2 - $CellContext`ky^2) $CellContext`mz)/($CellContext`k1 \
$CellContext`kt))/$CellContext`c) - ($CellContext`ky \
$CellContext`px)/$CellContext`kt + ($CellContext`kx \
$CellContext`py)/$CellContext`kt))/($CellContext`kz1 
     Pi^2), $CellContext`EpEpp = (((I/
        8) $CellContext`k1^2) ((-(($CellContext`ky \
$CellContext`mx)/$CellContext`kt) + ($CellContext`kx \
$CellContext`my)/$CellContext`kt)/$CellContext`c + (($CellContext`kx \
$CellContext`kz1) $CellContext`px)/($CellContext`k1 $CellContext`kt) + \
(($CellContext`ky $CellContext`kz1) $CellContext`py)/($CellContext`k1 \
$CellContext`kt) + ((-$CellContext`kx^2 - $CellContext`ky^2) \
$CellContext`pz)/($CellContext`k1 $CellContext`kt)))/($CellContext`kz1 
     Pi^2), $CellContext`EpEpm = (((I/
        8) $CellContext`k1^2) ((-(($CellContext`ky \
$CellContext`mx)/$CellContext`kt) + ($CellContext`kx \
$CellContext`my)/$CellContext`kt)/$CellContext`c - (($CellContext`kx \
$CellContext`kz1) $CellContext`px)/($CellContext`k1 $CellContext`kt) - \
(($CellContext`ky $CellContext`kz1) $CellContext`py)/($CellContext`k1 \
$CellContext`kt) + ((-$CellContext`kx^2 - $CellContext`ky^2) \
$CellContext`pz)/($CellContext`k1 $CellContext`kt)))/($CellContext`kz1 
     Pi^2)}]], "Output",
 Editable->False,
 CellOpen->False],

Cell[BoxData[
 DynamicBox[ToBoxes[
   If[$CellContext`point3 == False, 
    Dynamic[
     Which[
      And[$CellContext`pol3 == "spol", $CellContext`plane3 == 
       "zplus"], $CellContext`M3 = Coefficient[
        ReplaceAll[$CellContext`EsEpp, {$CellContext`kx -> $CellContext`kx3, \
$CellContext`ky -> $CellContext`ky3}], {$CellContext`px, $CellContext`py, \
$CellContext`pz, $CellContext`mx, $CellContext`my, $CellContext`mz}], 
      And[$CellContext`pol3 == "spol", $CellContext`plane3 == 
       "zmin"], $CellContext`M3 = Coefficient[
        ReplaceAll[$CellContext`EsEpm, {$CellContext`kx -> $CellContext`kx3, \
$CellContext`ky -> $CellContext`ky3}], {$CellContext`px, $CellContext`py, \
$CellContext`pz, $CellContext`mx, $CellContext`my, $CellContext`mz}], 
      And[$CellContext`pol3 == "ppol", $CellContext`plane3 == 
       "zplus"], $CellContext`M3 = Coefficient[
        ReplaceAll[$CellContext`EpEpp, {$CellContext`kx -> $CellContext`kx3, \
$CellContext`ky -> $CellContext`ky3}], {$CellContext`px, $CellContext`py, \
$CellContext`pz, $CellContext`mx, $CellContext`my, $CellContext`mz}], 
      And[$CellContext`pol3 == "ppol", $CellContext`plane3 == 
       "zmin"], $CellContext`M3 = Coefficient[
        ReplaceAll[$CellContext`EpEpm, {$CellContext`kx -> $CellContext`kx3, \
$CellContext`ky -> $CellContext`ky3}], {$CellContext`px, $CellContext`py, \
$CellContext`pz, $CellContext`mx, $CellContext`my, $CellContext`mz}]], 
     SaveDefinitions -> 
     True], $CellContext`M3 = {0, 0, 0, 0, 0, 0}; $CellContext`reA3 = 
     0, $CellContext`imA3 = 0], StandardForm],
  ImageSizeCache->{7., {0., 8.}},
  Initialization:>{$CellContext`EsEpp = ((I/
       8) (-((($CellContext`kx 
            Sqrt[1 - $CellContext`kx^2 - $CellContext`ky^2] $CellContext`mx)/
           Sqrt[$CellContext`kx^2 + $CellContext`ky^2] + ($CellContext`ky 
            Sqrt[1 - $CellContext`kx^2 - $CellContext`ky^2] $CellContext`my)/
           Sqrt[$CellContext`kx^2 + $CellContext`ky^2] + ((-$CellContext`kx^2 - \
$CellContext`ky^2) $CellContext`mz)/
           Sqrt[$CellContext`kx^2 + $CellContext`ky^2])/$CellContext`c) - \
($CellContext`ky $CellContext`px)/
       Sqrt[$CellContext`kx^2 + $CellContext`ky^2] + ($CellContext`kx \
$CellContext`py)/Sqrt[$CellContext`kx^2 + $CellContext`ky^2]))/(
     Sqrt[1 - $CellContext`kx^2 - $CellContext`ky^2] 
     Pi^2), $CellContext`EsEpm = ((I/
       8) (-((-(($CellContext`kx 
             Sqrt[1 - $CellContext`kx^2 - $CellContext`ky^2] $CellContext`mx)/
            Sqrt[$CellContext`kx^2 + $CellContext`ky^2]) - ($CellContext`ky 
           Sqrt[1 - $CellContext`kx^2 - $CellContext`ky^2] $CellContext`my)/
          Sqrt[$CellContext`kx^2 + $CellContext`ky^2] + ((-$CellContext`kx^2 - \
$CellContext`ky^2) $CellContext`mz)/
           Sqrt[$CellContext`kx^2 + $CellContext`ky^2])/$CellContext`c) - \
($CellContext`ky $CellContext`px)/
       Sqrt[$CellContext`kx^2 + $CellContext`ky^2] + ($CellContext`kx \
$CellContext`py)/Sqrt[$CellContext`kx^2 + $CellContext`ky^2]))/(
     Sqrt[1 - $CellContext`kx^2 - $CellContext`ky^2] 
     Pi^2), $CellContext`EpEpp = ((I/
       8) ((-(($CellContext`ky $CellContext`mx)/
           Sqrt[$CellContext`kx^2 + $CellContext`ky^2]) + ($CellContext`kx \
$CellContext`my)/
          Sqrt[$CellContext`kx^2 + $CellContext`ky^2])/$CellContext`c + \
($CellContext`kx 
         Sqrt[1 - $CellContext`kx^2 - $CellContext`ky^2] $CellContext`px)/
        Sqrt[$CellContext`kx^2 + $CellContext`ky^2] + ($CellContext`ky 
         Sqrt[1 - $CellContext`kx^2 - $CellContext`ky^2] $CellContext`py)/
        Sqrt[$CellContext`kx^2 + $CellContext`ky^2] + ((-$CellContext`kx^2 - \
$CellContext`ky^2) $CellContext`pz)/
        Sqrt[$CellContext`kx^2 + $CellContext`ky^2]))/(
     Sqrt[1 - $CellContext`kx^2 - $CellContext`ky^2] 
     Pi^2), $CellContext`EpEpm = ((I/
       8) ((-(($CellContext`ky $CellContext`mx)/
           Sqrt[$CellContext`kx^2 + $CellContext`ky^2]) + ($CellContext`kx \
$CellContext`my)/
          Sqrt[$CellContext`kx^2 + $CellContext`ky^2])/$CellContext`c - \
($CellContext`kx 
        Sqrt[1 - $CellContext`kx^2 - $CellContext`ky^2] $CellContext`px)/
       Sqrt[$CellContext`kx^2 + $CellContext`ky^2] - ($CellContext`ky 
        Sqrt[1 - $CellContext`kx^2 - $CellContext`ky^2] $CellContext`py)/
       Sqrt[$CellContext`kx^2 + $CellContext`ky^2] + ((-$CellContext`kx^2 - \
$CellContext`ky^2) $CellContext`pz)/
        Sqrt[$CellContext`kx^2 + $CellContext`ky^2]))/(
     Sqrt[1 - $CellContext`kx^2 - $CellContext`ky^2] Pi^2)}]], "Output",
 Editable->False,
 CellOpen->False],

Cell[BoxData[
 DynamicBox[ToBoxes[
   If[$CellContext`point4 == False, 
    Dynamic[
     Which[
      And[$CellContext`pol4 == "spol", $CellContext`plane4 == 
       "zplus"], $CellContext`M4 = Coefficient[
        ReplaceAll[$CellContext`EsEpp, {$CellContext`kx -> $CellContext`kx4, \
$CellContext`ky -> $CellContext`ky4}], {$CellContext`px, $CellContext`py, \
$CellContext`pz, $CellContext`mx, $CellContext`my, $CellContext`mz}], 
      And[$CellContext`pol4 == "spol", $CellContext`plane4 == 
       "zmin"], $CellContext`M4 = Coefficient[
        ReplaceAll[$CellContext`EsEpm, {$CellContext`kx -> $CellContext`kx4, \
$CellContext`ky -> $CellContext`ky4}], {$CellContext`px, $CellContext`py, \
$CellContext`pz, $CellContext`mx, $CellContext`my, $CellContext`mz}], 
      And[$CellContext`pol4 == "ppol", $CellContext`plane4 == 
       "zplus"], $CellContext`M4 = Coefficient[
        ReplaceAll[$CellContext`EpEpp, {$CellContext`kx -> $CellContext`kx4, \
$CellContext`ky -> $CellContext`ky4}], {$CellContext`px, $CellContext`py, \
$CellContext`pz, $CellContext`mx, $CellContext`my, $CellContext`mz}], 
      And[$CellContext`pol4 == "ppol", $CellContext`plane4 == 
       "zmin"], $CellContext`M4 = Coefficient[
        ReplaceAll[$CellContext`EpEpm, {$CellContext`kx -> $CellContext`kx4, \
$CellContext`ky -> $CellContext`ky4}], {$CellContext`px, $CellContext`py, \
$CellContext`pz, $CellContext`mx, $CellContext`my, $CellContext`mz}]], 
     SaveDefinitions -> 
     True], $CellContext`M4 = {0, 0, 0, 0, 0, 0}; $CellContext`reA4 = 
     0, $CellContext`imA4 = 0], StandardForm],
  ImageSizeCache->{7., {0., 8.}},
  Initialization:>{$CellContext`EsEpp = ((I/
       8) (-((($CellContext`kx 
            Sqrt[1 - $CellContext`kx^2 - $CellContext`ky^2] $CellContext`mx)/
           Sqrt[$CellContext`kx^2 + $CellContext`ky^2] + ($CellContext`ky 
            Sqrt[1 - $CellContext`kx^2 - $CellContext`ky^2] $CellContext`my)/
           Sqrt[$CellContext`kx^2 + $CellContext`ky^2] + ((-$CellContext`kx^2 - \
$CellContext`ky^2) $CellContext`mz)/
           Sqrt[$CellContext`kx^2 + $CellContext`ky^2])/$CellContext`c) - \
($CellContext`ky $CellContext`px)/
       Sqrt[$CellContext`kx^2 + $CellContext`ky^2] + ($CellContext`kx \
$CellContext`py)/Sqrt[$CellContext`kx^2 + $CellContext`ky^2]))/(
     Sqrt[1 - $CellContext`kx^2 - $CellContext`ky^2] 
     Pi^2), $CellContext`EsEpm = ((I/
       8) (-((-(($CellContext`kx 
             Sqrt[1 - $CellContext`kx^2 - $CellContext`ky^2] $CellContext`mx)/
            Sqrt[$CellContext`kx^2 + $CellContext`ky^2]) - ($CellContext`ky 
           Sqrt[1 - $CellContext`kx^2 - $CellContext`ky^2] $CellContext`my)/
          Sqrt[$CellContext`kx^2 + $CellContext`ky^2] + ((-$CellContext`kx^2 - \
$CellContext`ky^2) $CellContext`mz)/
           Sqrt[$CellContext`kx^2 + $CellContext`ky^2])/$CellContext`c) - \
($CellContext`ky $CellContext`px)/
       Sqrt[$CellContext`kx^2 + $CellContext`ky^2] + ($CellContext`kx \
$CellContext`py)/Sqrt[$CellContext`kx^2 + $CellContext`ky^2]))/(
     Sqrt[1 - $CellContext`kx^2 - $CellContext`ky^2] 
     Pi^2), $CellContext`EpEpp = ((I/
       8) ((-(($CellContext`ky $CellContext`mx)/
           Sqrt[$CellContext`kx^2 + $CellContext`ky^2]) + ($CellContext`kx \
$CellContext`my)/
          Sqrt[$CellContext`kx^2 + $CellContext`ky^2])/$CellContext`c + \
($CellContext`kx 
         Sqrt[1 - $CellContext`kx^2 - $CellContext`ky^2] $CellContext`px)/
        Sqrt[$CellContext`kx^2 + $CellContext`ky^2] + ($CellContext`ky 
         Sqrt[1 - $CellContext`kx^2 - $CellContext`ky^2] $CellContext`py)/
        Sqrt[$CellContext`kx^2 + $CellContext`ky^2] + ((-$CellContext`kx^2 - \
$CellContext`ky^2) $CellContext`pz)/
        Sqrt[$CellContext`kx^2 + $CellContext`ky^2]))/(
     Sqrt[1 - $CellContext`kx^2 - $CellContext`ky^2] 
     Pi^2), $CellContext`EpEpm = ((I/
       8) ((-(($CellContext`ky $CellContext`mx)/
           Sqrt[$CellContext`kx^2 + $CellContext`ky^2]) + ($CellContext`kx \
$CellContext`my)/
          Sqrt[$CellContext`kx^2 + $CellContext`ky^2])/$CellContext`c - \
($CellContext`kx 
        Sqrt[1 - $CellContext`kx^2 - $CellContext`ky^2] $CellContext`px)/
       Sqrt[$CellContext`kx^2 + $CellContext`ky^2] - ($CellContext`ky 
        Sqrt[1 - $CellContext`kx^2 - $CellContext`ky^2] $CellContext`py)/
       Sqrt[$CellContext`kx^2 + $CellContext`ky^2] + ((-$CellContext`kx^2 - \
$CellContext`ky^2) $CellContext`pz)/
        Sqrt[$CellContext`kx^2 + $CellContext`ky^2]))/(
     Sqrt[1 - $CellContext`kx^2 - $CellContext`ky^2] Pi^2), $CellContext`imA4 = 
    0}]], "Output",
 Editable->False,
 CellOpen->False],

Cell[BoxData[
 DynamicBox[ToBoxes[
   If[$CellContext`point5 == False, 
    Dynamic[
     Which[
      And[$CellContext`pol5 == "spol", $CellContext`plane5 == 
       "zplus"], $CellContext`M5 = Coefficient[
        ReplaceAll[$CellContext`EsEpp, {$CellContext`kx -> $CellContext`kx5, \
$CellContext`ky -> $CellContext`ky5}], {$CellContext`px, $CellContext`py, \
$CellContext`pz, $CellContext`mx, $CellContext`my, $CellContext`mz}], 
      And[$CellContext`pol5 == "spol", $CellContext`plane5 == 
       "zmin"], $CellContext`M5 = Coefficient[
        ReplaceAll[$CellContext`EsEpm, {$CellContext`kx -> $CellContext`kx5, \
$CellContext`ky -> $CellContext`ky5}], {$CellContext`px, $CellContext`py, \
$CellContext`pz, $CellContext`mx, $CellContext`my, $CellContext`mz}], 
      And[$CellContext`pol5 == "ppol", $CellContext`plane5 == 
       "zplus"], $CellContext`M5 = Coefficient[
        ReplaceAll[$CellContext`EpEpp, {$CellContext`kx -> $CellContext`kx5, \
$CellContext`ky -> $CellContext`ky5}], {$CellContext`px, $CellContext`py, \
$CellContext`pz, $CellContext`mx, $CellContext`my, $CellContext`mz}], 
      And[$CellContext`pol5 == "ppol", $CellContext`plane5 == 
       "zmin"], $CellContext`M5 = Coefficient[
        ReplaceAll[$CellContext`EpEpm, {$CellContext`kx -> $CellContext`kx5, \
$CellContext`ky -> $CellContext`ky5}], {$CellContext`px, $CellContext`py, \
$CellContext`pz, $CellContext`mx, $CellContext`my, $CellContext`mz}]], 
     SaveDefinitions -> 
     True], $CellContext`M5 = {0, 0, 0, 0, 0, 0}; $CellContext`reA5 = 
     0, $CellContext`imA5 = 0], StandardForm],
  ImageSizeCache->{7., {0., 8.}},
  Initialization:>{$CellContext`EsEpp = ((I/
       8) (-((($CellContext`kx 
            Sqrt[1 - $CellContext`kx^2 - $CellContext`ky^2] $CellContext`mx)/
           Sqrt[$CellContext`kx^2 + $CellContext`ky^2] + ($CellContext`ky 
            Sqrt[1 - $CellContext`kx^2 - $CellContext`ky^2] $CellContext`my)/
           Sqrt[$CellContext`kx^2 + $CellContext`ky^2] + ((-$CellContext`kx^2 - \
$CellContext`ky^2) $CellContext`mz)/
           Sqrt[$CellContext`kx^2 + $CellContext`ky^2])/$CellContext`c) - \
($CellContext`ky $CellContext`px)/
       Sqrt[$CellContext`kx^2 + $CellContext`ky^2] + ($CellContext`kx \
$CellContext`py)/Sqrt[$CellContext`kx^2 + $CellContext`ky^2]))/(
     Sqrt[1 - $CellContext`kx^2 - $CellContext`ky^2] 
     Pi^2), $CellContext`EsEpm = ((I/
       8) (-((-(($CellContext`kx 
             Sqrt[1 - $CellContext`kx^2 - $CellContext`ky^2] $CellContext`mx)/
            Sqrt[$CellContext`kx^2 + $CellContext`ky^2]) - ($CellContext`ky 
           Sqrt[1 - $CellContext`kx^2 - $CellContext`ky^2] $CellContext`my)/
          Sqrt[$CellContext`kx^2 + $CellContext`ky^2] + ((-$CellContext`kx^2 - \
$CellContext`ky^2) $CellContext`mz)/
           Sqrt[$CellContext`kx^2 + $CellContext`ky^2])/$CellContext`c) - \
($CellContext`ky $CellContext`px)/
       Sqrt[$CellContext`kx^2 + $CellContext`ky^2] + ($CellContext`kx \
$CellContext`py)/Sqrt[$CellContext`kx^2 + $CellContext`ky^2]))/(
     Sqrt[1 - $CellContext`kx^2 - $CellContext`ky^2] 
     Pi^2), $CellContext`EpEpp = ((I/
       8) ((-(($CellContext`ky $CellContext`mx)/
           Sqrt[$CellContext`kx^2 + $CellContext`ky^2]) + ($CellContext`kx \
$CellContext`my)/
          Sqrt[$CellContext`kx^2 + $CellContext`ky^2])/$CellContext`c + \
($CellContext`kx 
         Sqrt[1 - $CellContext`kx^2 - $CellContext`ky^2] $CellContext`px)/
        Sqrt[$CellContext`kx^2 + $CellContext`ky^2] + ($CellContext`ky 
         Sqrt[1 - $CellContext`kx^2 - $CellContext`ky^2] $CellContext`py)/
        Sqrt[$CellContext`kx^2 + $CellContext`ky^2] + ((-$CellContext`kx^2 - \
$CellContext`ky^2) $CellContext`pz)/
        Sqrt[$CellContext`kx^2 + $CellContext`ky^2]))/(
     Sqrt[1 - $CellContext`kx^2 - $CellContext`ky^2] 
     Pi^2), $CellContext`EpEpm = ((I/
       8) ((-(($CellContext`ky $CellContext`mx)/
           Sqrt[$CellContext`kx^2 + $CellContext`ky^2]) + ($CellContext`kx \
$CellContext`my)/
          Sqrt[$CellContext`kx^2 + $CellContext`ky^2])/$CellContext`c - \
($CellContext`kx 
        Sqrt[1 - $CellContext`kx^2 - $CellContext`ky^2] $CellContext`px)/
       Sqrt[$CellContext`kx^2 + $CellContext`ky^2] - ($CellContext`ky 
        Sqrt[1 - $CellContext`kx^2 - $CellContext`ky^2] $CellContext`py)/
       Sqrt[$CellContext`kx^2 + $CellContext`ky^2] + ((-$CellContext`kx^2 - \
$CellContext`ky^2) $CellContext`pz)/
        Sqrt[$CellContext`kx^2 + $CellContext`ky^2]))/(
     Sqrt[1 - $CellContext`kx^2 - $CellContext`ky^2] Pi^2), $CellContext`imA5 = 
    0}]], "Output",
 Editable->False,
 CellOpen->False],

Cell[BoxData[
 DynamicBox[ToBoxes[
   If[$CellContext`point6 == False, 
    Dynamic[
     Which[
      And[$CellContext`pol6 == "spol", $CellContext`plane6 == 
       "zplus"], $CellContext`M6 = Coefficient[
        ReplaceAll[$CellContext`EsEpp, {$CellContext`kx -> $CellContext`kx6, \
$CellContext`ky -> $CellContext`ky6}], {$CellContext`px, $CellContext`py, \
$CellContext`pz, $CellContext`mx, $CellContext`my, $CellContext`mz}], 
      And[$CellContext`pol6 == "spol", $CellContext`plane6 == 
       "zmin"], $CellContext`M6 = Coefficient[
        ReplaceAll[$CellContext`EsEpm, {$CellContext`kx -> $CellContext`kx6, \
$CellContext`ky -> $CellContext`ky6}], {$CellContext`px, $CellContext`py, \
$CellContext`pz, $CellContext`mx, $CellContext`my, $CellContext`mz}], 
      And[$CellContext`pol6 == "ppol", $CellContext`plane6 == 
       "zplus"], $CellContext`M6 = Coefficient[
        ReplaceAll[$CellContext`EpEpp, {$CellContext`kx -> $CellContext`kx6, \
$CellContext`ky -> $CellContext`ky6}], {$CellContext`px, $CellContext`py, \
$CellContext`pz, $CellContext`mx, $CellContext`my, $CellContext`mz}], 
      And[$CellContext`pol6 == "ppol", $CellContext`plane6 == 
       "zmin"], $CellContext`M6 = Coefficient[
        ReplaceAll[$CellContext`EpEpm, {$CellContext`kx -> $CellContext`kx6, \
$CellContext`ky -> $CellContext`ky6}], {$CellContext`px, $CellContext`py, \
$CellContext`pz, $CellContext`mx, $CellContext`my, $CellContext`mz}]], 
     SaveDefinitions -> 
     True], $CellContext`M6 = {0, 0, 0, 0, 0, 0}; $CellContext`reA6 = 
     0; $CellContext`imA6 = 0], StandardForm],
  ImageSizeCache->{7., {0., 8.}},
  Initialization:>{$CellContext`EsEpp = ((I/
       8) (-(((($CellContext`kx 
             Sqrt[1 - $CellContext`kx^2 - $CellContext`ky^2]) \
$CellContext`mx)/
           Sqrt[$CellContext`kx^2 + $CellContext`ky^2] + (($CellContext`ky 
             Sqrt[1 - $CellContext`kx^2 - $CellContext`ky^2]) \
$CellContext`my)/
           Sqrt[$CellContext`kx^2 + $CellContext`ky^2] + ((-$CellContext`kx^2 - \
$CellContext`ky^2) $CellContext`mz)/
           Sqrt[$CellContext`kx^2 + $CellContext`ky^2])/$CellContext`c) - \
($CellContext`ky $CellContext`px)/
       Sqrt[$CellContext`kx^2 + $CellContext`ky^2] + ($CellContext`kx \
$CellContext`py)/Sqrt[$CellContext`kx^2 + $CellContext`ky^2]))/(
     Sqrt[1 - $CellContext`kx^2 - $CellContext`ky^2] 
     Pi^2), $CellContext`EsEpm = ((I/
       8) (-((-((($CellContext`kx 
              Sqrt[1 - $CellContext`kx^2 - $CellContext`ky^2]) \
$CellContext`mx)/
            Sqrt[$CellContext`kx^2 + $CellContext`ky^2]) - (($CellContext`ky 
            Sqrt[1 - $CellContext`kx^2 - $CellContext`ky^2]) $CellContext`my)/
          Sqrt[$CellContext`kx^2 + $CellContext`ky^2] + ((-$CellContext`kx^2 - \
$CellContext`ky^2) $CellContext`mz)/
           Sqrt[$CellContext`kx^2 + $CellContext`ky^2])/$CellContext`c) - \
($CellContext`ky $CellContext`px)/
       Sqrt[$CellContext`kx^2 + $CellContext`ky^2] + ($CellContext`kx \
$CellContext`py)/Sqrt[$CellContext`kx^2 + $CellContext`ky^2]))/(
     Sqrt[1 - $CellContext`kx^2 - $CellContext`ky^2] 
     Pi^2), $CellContext`EpEpp = ((I/
       8) ((-(($CellContext`ky $CellContext`mx)/
           Sqrt[$CellContext`kx^2 + $CellContext`ky^2]) + ($CellContext`kx \
$CellContext`my)/
          Sqrt[$CellContext`kx^2 + $CellContext`ky^2])/$CellContext`c + \
(($CellContext`kx 
          Sqrt[1 - $CellContext`kx^2 - $CellContext`ky^2]) $CellContext`px)/
        Sqrt[$CellContext`kx^2 + $CellContext`ky^2] + (($CellContext`ky 
          Sqrt[1 - $CellContext`kx^2 - $CellContext`ky^2]) $CellContext`py)/
        Sqrt[$CellContext`kx^2 + $CellContext`ky^2] + ((-$CellContext`kx^2 - \
$CellContext`ky^2) $CellContext`pz)/
        Sqrt[$CellContext`kx^2 + $CellContext`ky^2]))/(
     Sqrt[1 - $CellContext`kx^2 - $CellContext`ky^2] 
     Pi^2), $CellContext`EpEpm = ((I/
       8) ((-(($CellContext`ky $CellContext`mx)/
           Sqrt[$CellContext`kx^2 + $CellContext`ky^2]) + ($CellContext`kx \
$CellContext`my)/
          Sqrt[$CellContext`kx^2 + $CellContext`ky^2])/$CellContext`c - \
(($CellContext`kx 
         Sqrt[1 - $CellContext`kx^2 - $CellContext`ky^2]) $CellContext`px)/
       Sqrt[$CellContext`kx^2 + $CellContext`ky^2] - (($CellContext`ky 
         Sqrt[1 - $CellContext`kx^2 - $CellContext`ky^2]) $CellContext`py)/
       Sqrt[$CellContext`kx^2 + $CellContext`ky^2] + ((-$CellContext`kx^2 - \
$CellContext`ky^2) $CellContext`pz)/
        Sqrt[$CellContext`kx^2 + $CellContext`ky^2]))/(
     Sqrt[1 - $CellContext`kx^2 - $CellContext`ky^2] Pi^2)}]], "Output",
 Editable->False,
 CellOpen->False],

Cell[BoxData[
 DynamicBox[
  ToBoxes[$CellContext`sol = 
   LinearSolve[{$CellContext`M1, $CellContext`M2, $CellContext`M3, \
$CellContext`M4, $CellContext`M5, $CellContext`M6}, 
     FullSimplify[{$CellContext`reA1 + I $CellContext`imA1, $CellContext`reA2 + 
       I $CellContext`imA2, $CellContext`reA3 + 
       I $CellContext`imA3, $CellContext`reA4 + 
       I $CellContext`imA4, $CellContext`reA5 + 
       I $CellContext`imA5, $CellContext`reA6 + I $CellContext`imA6}]], 
   StandardForm],
  ImageSizeCache->{113., {2., 8.}},
  Initialization:>{$CellContext`imA3 = 0, $CellContext`imA4 = 
    0, $CellContext`imA5 = 0}]], "Output",
 Editable->False,
 CellOpen->False],

Cell[BoxData[
 DynamicBox[
  ToBoxes[$CellContext`nulsol = 
   NullSpace[{$CellContext`M1, $CellContext`M2, $CellContext`M3, \
$CellContext`M4, $CellContext`M5, $CellContext`M6}], StandardForm],
  ImageSizeCache->{974.8, {13.1, 29.}}]], "Output",
 Editable->False,
 CellOpen->False],

Cell[BoxData[
 DynamicBox[ToBoxes[
   Which[Length[$CellContext`nulsol] == 
    0, $CellContext`coef = " "; $CellContext`plus = " "; $CellContext`with = 
     " "; $CellContext`compl = " "; $CellContext`ToPrint = " ", 
    Length[$CellContext`nulsol] == 
    1, $CellContext`coef = {$CellContext`\[Alpha]}; $CellContext`plus = 
     "+"; $CellContext`with = "With"; $CellContext`compl = 
     "\[Element]\[DoubleStruckCapitalC]"; $CellContext`ToPrint = MatrixForm[
        Part[$CellContext`coef, 1]] MatrixForm[
        Part[$CellContext`nulsol, 1]], Length[$CellContext`nulsol] == 
    2, $CellContext`coef = {$CellContext`\[Alpha], $CellContext`\[Beta]}; \
$CellContext`plus = "+"; $CellContext`with = "With"; $CellContext`compl = 
     "\[Element]\[DoubleStruckCapitalC]"; $CellContext`ToPrint = Dynamic[
         MatrixForm[
          Part[$CellContext`coef, 1]]] Dynamic[
         MatrixForm[
          Part[$CellContext`nulsol, 1]]] + Dynamic[
         MatrixForm[
          Part[$CellContext`coef, 2]]] Dynamic[
         MatrixForm[
          Part[$CellContext`nulsol, 2]]], Length[$CellContext`nulsol] == 
    3, $CellContext`coef = {$CellContext`\[Alpha], $CellContext`\[Beta], \
$CellContext`\[Gamma]}; $CellContext`plus = "+"; $CellContext`with = 
     "With"; $CellContext`compl = 
     "\[Element]\[DoubleStruckCapitalC]"; $CellContext`ToPrint = Dynamic[
         MatrixForm[
          Part[$CellContext`coef, 1]]] Dynamic[
         MatrixForm[
          Part[$CellContext`nulsol, 1]]] + Dynamic[
         MatrixForm[
          Part[$CellContext`coef, 2]]] Dynamic[
         MatrixForm[
          Part[$CellContext`nulsol, 2]]] + Dynamic[
         MatrixForm[
          Part[$CellContext`coef, 3]]] Dynamic[
         MatrixForm[
          Part[$CellContext`nulsol, 3]]], Length[$CellContext`nulsol] == 
    4, $CellContext`coef = {$CellContext`\[Alpha], $CellContext`\[Beta], \
$CellContext`\[Gamma], $CellContext`\[Delta]}; $CellContext`plus = 
     "+"; $CellContext`with = "With"; $CellContext`compl = 
     "\[Element]\[DoubleStruckCapitalC]"; $CellContext`ToPrint = Dynamic[
         MatrixForm[
          Part[$CellContext`coef, 1]]] Dynamic[
         MatrixForm[
          Part[$CellContext`nulsol, 1]]] + Dynamic[
         MatrixForm[
          Part[$CellContext`coef, 2]]] Dynamic[
         MatrixForm[
          Part[$CellContext`nulsol, 2]]] + Dynamic[
         MatrixForm[
          Part[$CellContext`coef, 3]]] Dynamic[
         MatrixForm[
          Part[$CellContext`nulsol, 3]]] + Dynamic[
         MatrixForm[
          Part[$CellContext`coef, 4]]] Dynamic[
         MatrixForm[
          Part[$CellContext`nulsol, 4]]], Length[$CellContext`nulsol] == 
    5, $CellContext`coef = {$CellContext`\[Alpha], $CellContext`\[Beta], \
$CellContext`\[Gamma], $CellContext`\[Delta], $CellContext`\[Epsilon]}; \
$CellContext`plus = "+"; $CellContext`with = "With"; $CellContext`compl = 
     "\[Element]\[DoubleStruckCapitalC]"; $CellContext`ToPrint = Dynamic[
         MatrixForm[
          Part[$CellContext`coef, 1]]] Dynamic[
         MatrixForm[
          Part[$CellContext`nulsol, 1]]] + Dynamic[
         MatrixForm[
          Part[$CellContext`coef, 2]]] Dynamic[
         MatrixForm[
          Part[$CellContext`nulsol, 2]]] + Dynamic[
         MatrixForm[
          Part[$CellContext`coef, 3]]] Dynamic[
         MatrixForm[
          Part[$CellContext`nulsol, 3]]] + Dynamic[
         MatrixForm[
          Part[$CellContext`coef, 4]]] Dynamic[
         MatrixForm[
          Part[$CellContext`nulsol, 4]]] + Dynamic[
         MatrixForm[
          Part[$CellContext`coef, 5]]] Dynamic[
         MatrixForm[
          Part[$CellContext`nulsol, 5]]]], StandardForm],
  ImageSizeCache->{501., {59., 66.}}]], "Output",
 Editable->False,
 CellOpen->False],

Cell[BoxData[
 FrameBox[
  StyleBox["\<\"Insert here the normalised wavevectors for each of the points \
you want to fix in the electric field spectra, the polarisation and \
half-space for each of the points and the complex amplitudes of the spectra \
at said points.\\nThe code returns the values for the 6 dipole moments \
components: \\!\\(\\*SubscriptBox[\\(p\\), \\(x\\)]\\), \
\\!\\(\\*SubscriptBox[\\(p\\), \\(y\\)]\\), \\!\\(\\*SubscriptBox[\\(p\\), \
\\(z\\)]\\), \\!\\(\\*SubscriptBox[\\(m\\), \\(x\\)]\\), \
\\!\\(\\*SubscriptBox[\\(m\\), \\(y\\)]\\), \\!\\(\\*SubscriptBox[\\(m\\), \
\\(z\\)]\\) as a particular solution + the associated homogeneous system's \
solutions, each multiplied by an arbitrary complex coefficient.\\nThe code is \
solving the linear system for the electric field angular spectrum with a \
value of \\!\\(\\*SubscriptBox[\\(k\\), \\(0\\)]\\)=\\!\\(\\*FractionBox[\\(2 \
 \[Pi]\\), \\(\[Lambda]\\)]\\)=1, so the dipoles are normalised by a factor \
of \\!\\(\\*SubscriptBox[\\(k\\), \\(0\\)]\\)\"\>",
   StripOnInput->False,
   LineColor->RGBColor[0, 0, 1],
   FrontFaceColor->RGBColor[0, 0, 1],
   BackFaceColor->RGBColor[0, 0, 1],
   GraphicsColor->RGBColor[0, 0, 1],
   FontFamily->"Helvetica",
   FontColor->RGBColor[0, 0, 1]],
  StripOnInput->False]], "Output"],

Cell[BoxData[
 TagBox[GridBox[{
    {"\<\" \"\>", "\<\" \"\>", 
     StyleBox["\<\"Normalised \\!\\(\\*SubscriptBox[\\(k\\), \\(x\\)]\\) (\\!\
\\(\\*SubscriptBox[\\(k\\), \\(x\\)]\\)/\\!\\(\\*SubscriptBox[\\(k\\), \
\\(0\\)]\\))\"\>",
      StripOnInput->False,
      FontFamily->"Helvetica",
      FontSize->Larger,
      FontWeight->Bold], 
     StyleBox["\<\"Normalised (\\!\\(\\*SubscriptBox[\\(k\\), \\(y\\)]\\)/\\!\
\\(\\*SubscriptBox[\\(k\\), \\(0\\)]\\))\"\>",
      StripOnInput->False,
      FontFamily->"Helvetica",
      FontSize->Larger,
      FontWeight->Bold], 
     StyleBox["\<\"Select polarisation\"\>",
      StripOnInput->False,
      FontFamily->"Helvetica",
      FontSize->Larger,
      FontWeight->Bold], 
     StyleBox["\<\"Select half-space\"\>",
      StripOnInput->False,
      FontFamily->"Helvetica",
      FontSize->Larger,
      FontWeight->Bold], 
     StyleBox["\<\"Complex amplitude (A+iB)\"\>",
      StripOnInput->False,
      FontFamily->"Helvetica",
      FontSize->Larger,
      FontWeight->Bold], "\[SpanFromLeft]", "\[SpanFromLeft]"},
    {
     CheckboxBox[Dynamic[$CellContext`point1], {True, False}], 
     StyleBox["\<\"1st point\"\>",
      StripOnInput->False,
      FontFamily->"Helvetica"], 
     InputFieldBox[Dynamic[$CellContext`kx1], Number,
      FieldHint->"kx1/k0",
      FieldSize->Tiny], 
     InputFieldBox[Dynamic[$CellContext`ky1], Number,
      FieldHint->"ky1/k0",
      FieldSize->Tiny], 
     InterpretationBox[
      StyleBox[
       RowBox[{GridBox[{
          {
           RadioButtonBox[Dynamic[$CellContext`pol1], {"spol"},
            DefaultBaseStyle->"RadioButtonBar"], 
           StyleBox["\<\"s\"\>", "RadioButtonBarLabel",
            StripOnInput->False]}
         },
         AutoDelete->False,
         GridBoxAlignment->{
          "Columns" -> {{Left}}, "ColumnsIndexed" -> {}, 
           "Rows" -> {{Baseline}}, "RowsIndexed" -> {}},
         GridBoxItemSize->{
          "Columns" -> {{Automatic}}, "Rows" -> {{Automatic}}},
         GridBoxSpacings->{"Columns" -> {
             Offset[0.27999999999999997`], {
              Offset[0.21]}, 
             Offset[0.27999999999999997`]}, "ColumnsIndexed" -> {}, "Rows" -> {
             Offset[0.2], {
              Offset[0.4]}, 
             Offset[0.2]}, "RowsIndexed" -> {}}], "  ", GridBox[{
          {
           RadioButtonBox[Dynamic[$CellContext`pol1], {"ppol"},
            DefaultBaseStyle->"RadioButtonBar"], 
           StyleBox["\<\"p\"\>", "RadioButtonBarLabel",
            StripOnInput->False]}
         },
         AutoDelete->False,
         GridBoxAlignment->{
          "Columns" -> {{Left}}, "ColumnsIndexed" -> {}, 
           "Rows" -> {{Baseline}}, "RowsIndexed" -> {}},
         GridBoxItemSize->{
          "Columns" -> {{Automatic}}, "Rows" -> {{Automatic}}},
         GridBoxSpacings->{"Columns" -> {
             Offset[0.27999999999999997`], {
              Offset[0.21]}, 
             Offset[0.27999999999999997`]}, "ColumnsIndexed" -> {}, "Rows" -> {
             Offset[0.2], {
              Offset[0.4]}, 
             Offset[0.2]}, "RowsIndexed" -> {}}]}], "Deploy"],
      RadioButtonBar[
       Dynamic[$CellContext`pol1], {"spol" -> "s", "ppol" -> "p"}]], 
     InterpretationBox[
      StyleBox[
       RowBox[{GridBox[{
          {
           RadioButtonBox[Dynamic[$CellContext`plane1], {"zplus"},
            DefaultBaseStyle->"RadioButtonBar"], 
           StyleBox["\<\"z>0\"\>", "RadioButtonBarLabel",
            StripOnInput->False]}
         },
         AutoDelete->False,
         GridBoxAlignment->{
          "Columns" -> {{Left}}, "ColumnsIndexed" -> {}, 
           "Rows" -> {{Baseline}}, "RowsIndexed" -> {}},
         GridBoxItemSize->{
          "Columns" -> {{Automatic}}, "Rows" -> {{Automatic}}},
         GridBoxSpacings->{"Columns" -> {
             Offset[0.27999999999999997`], {
              Offset[0.21]}, 
             Offset[0.27999999999999997`]}, "ColumnsIndexed" -> {}, "Rows" -> {
             Offset[0.2], {
              Offset[0.4]}, 
             Offset[0.2]}, "RowsIndexed" -> {}}], "  ", GridBox[{
          {
           RadioButtonBox[Dynamic[$CellContext`plane1], {"zmin"},
            DefaultBaseStyle->"RadioButtonBar"], 
           StyleBox["\<\"z<0\"\>", "RadioButtonBarLabel",
            StripOnInput->False]}
         },
         AutoDelete->False,
         GridBoxAlignment->{
          "Columns" -> {{Left}}, "ColumnsIndexed" -> {}, 
           "Rows" -> {{Baseline}}, "RowsIndexed" -> {}},
         GridBoxItemSize->{
          "Columns" -> {{Automatic}}, "Rows" -> {{Automatic}}},
         GridBoxSpacings->{"Columns" -> {
             Offset[0.27999999999999997`], {
              Offset[0.21]}, 
             Offset[0.27999999999999997`]}, "ColumnsIndexed" -> {}, "Rows" -> {
             Offset[0.2], {
              Offset[0.4]}, 
             Offset[0.2]}, "RowsIndexed" -> {}}]}], "Deploy"],
      RadioButtonBar[
       Dynamic[$CellContext`plane1], {"zplus" -> "z>0", "zmin" -> "z<0"}]], 
     InputFieldBox[Dynamic[$CellContext`reA1, SaveDefinitions -> True], Number,
      FieldHint->"Re[A1]",
      FieldSize->Tiny], "\<\"+\[ImaginaryI]\"\>", 
     InputFieldBox[Dynamic[$CellContext`imA1, SaveDefinitions -> True], Number,
      FieldHint->"Im[A1]",
      FieldSize->Tiny]},
    {
     CheckboxBox[Dynamic[$CellContext`point2], {True, False}], 
     StyleBox["\<\"2nd point\"\>",
      StripOnInput->False,
      FontFamily->"Helvetica"], 
     InputFieldBox[Dynamic[$CellContext`kx2], Number,
      FieldHint->"kx2/k0",
      FieldSize->Tiny], 
     InputFieldBox[Dynamic[$CellContext`ky2], Number,
      FieldHint->"ky2/k0",
      FieldSize->Tiny], 
     InterpretationBox[
      StyleBox[
       RowBox[{GridBox[{
          {
           RadioButtonBox[Dynamic[$CellContext`pol2], {"spol"},
            DefaultBaseStyle->"RadioButtonBar"], 
           StyleBox["\<\"s\"\>", "RadioButtonBarLabel",
            StripOnInput->False]}
         },
         AutoDelete->False,
         GridBoxAlignment->{
          "Columns" -> {{Left}}, "ColumnsIndexed" -> {}, 
           "Rows" -> {{Baseline}}, "RowsIndexed" -> {}},
         GridBoxItemSize->{
          "Columns" -> {{Automatic}}, "Rows" -> {{Automatic}}},
         GridBoxSpacings->{"Columns" -> {
             Offset[0.27999999999999997`], {
              Offset[0.21]}, 
             Offset[0.27999999999999997`]}, "ColumnsIndexed" -> {}, "Rows" -> {
             Offset[0.2], {
              Offset[0.4]}, 
             Offset[0.2]}, "RowsIndexed" -> {}}], "  ", GridBox[{
          {
           RadioButtonBox[Dynamic[$CellContext`pol2], {"ppol"},
            DefaultBaseStyle->"RadioButtonBar"], 
           StyleBox["\<\"p\"\>", "RadioButtonBarLabel",
            StripOnInput->False]}
         },
         AutoDelete->False,
         GridBoxAlignment->{
          "Columns" -> {{Left}}, "ColumnsIndexed" -> {}, 
           "Rows" -> {{Baseline}}, "RowsIndexed" -> {}},
         GridBoxItemSize->{
          "Columns" -> {{Automatic}}, "Rows" -> {{Automatic}}},
         GridBoxSpacings->{"Columns" -> {
             Offset[0.27999999999999997`], {
              Offset[0.21]}, 
             Offset[0.27999999999999997`]}, "ColumnsIndexed" -> {}, "Rows" -> {
             Offset[0.2], {
              Offset[0.4]}, 
             Offset[0.2]}, "RowsIndexed" -> {}}]}], "Deploy"],
      RadioButtonBar[
       Dynamic[$CellContext`pol2], {"spol" -> "s", "ppol" -> "p"}]], 
     InterpretationBox[
      StyleBox[
       RowBox[{GridBox[{
          {
           RadioButtonBox[Dynamic[$CellContext`plane2], {"zplus"},
            DefaultBaseStyle->"RadioButtonBar"], 
           StyleBox["\<\"z>0\"\>", "RadioButtonBarLabel",
            StripOnInput->False]}
         },
         AutoDelete->False,
         GridBoxAlignment->{
          "Columns" -> {{Left}}, "ColumnsIndexed" -> {}, 
           "Rows" -> {{Baseline}}, "RowsIndexed" -> {}},
         GridBoxItemSize->{
          "Columns" -> {{Automatic}}, "Rows" -> {{Automatic}}},
         GridBoxSpacings->{"Columns" -> {
             Offset[0.27999999999999997`], {
              Offset[0.21]}, 
             Offset[0.27999999999999997`]}, "ColumnsIndexed" -> {}, "Rows" -> {
             Offset[0.2], {
              Offset[0.4]}, 
             Offset[0.2]}, "RowsIndexed" -> {}}], "  ", GridBox[{
          {
           RadioButtonBox[Dynamic[$CellContext`plane2], {"zmin"},
            DefaultBaseStyle->"RadioButtonBar"], 
           StyleBox["\<\"z<0\"\>", "RadioButtonBarLabel",
            StripOnInput->False]}
         },
         AutoDelete->False,
         GridBoxAlignment->{
          "Columns" -> {{Left}}, "ColumnsIndexed" -> {}, 
           "Rows" -> {{Baseline}}, "RowsIndexed" -> {}},
         GridBoxItemSize->{
          "Columns" -> {{Automatic}}, "Rows" -> {{Automatic}}},
         GridBoxSpacings->{"Columns" -> {
             Offset[0.27999999999999997`], {
              Offset[0.21]}, 
             Offset[0.27999999999999997`]}, "ColumnsIndexed" -> {}, "Rows" -> {
             Offset[0.2], {
              Offset[0.4]}, 
             Offset[0.2]}, "RowsIndexed" -> {}}]}], "Deploy"],
      RadioButtonBar[
       Dynamic[$CellContext`plane2], {"zplus" -> "z>0", "zmin" -> "z<0"}]], 
     InputFieldBox[Dynamic[$CellContext`reA2, SaveDefinitions -> True], Number,
      FieldHint->"Re[A2]",
      FieldSize->Tiny], "\<\"+\[ImaginaryI]\"\>", 
     InputFieldBox[Dynamic[$CellContext`imA2, SaveDefinitions -> True], Number,
      FieldHint->"Im[A2]",
      FieldSize->Tiny]},
    {
     CheckboxBox[Dynamic[$CellContext`point3], {True, False}], 
     StyleBox["\<\"3rd point\"\>",
      StripOnInput->False,
      FontFamily->"Helvetica"], 
     InputFieldBox[Dynamic[$CellContext`kx3], Number,
      FieldHint->"kx3/k0",
      FieldSize->Tiny], 
     InputFieldBox[Dynamic[$CellContext`ky3], Number,
      FieldHint->"ky3/k0",
      FieldSize->Tiny], 
     InterpretationBox[
      StyleBox[
       RowBox[{GridBox[{
          {
           RadioButtonBox[Dynamic[$CellContext`pol3], {"spol"},
            DefaultBaseStyle->"RadioButtonBar"], 
           StyleBox["\<\"s\"\>", "RadioButtonBarLabel",
            StripOnInput->False]}
         },
         AutoDelete->False,
         GridBoxAlignment->{
          "Columns" -> {{Left}}, "ColumnsIndexed" -> {}, 
           "Rows" -> {{Baseline}}, "RowsIndexed" -> {}},
         GridBoxItemSize->{
          "Columns" -> {{Automatic}}, "Rows" -> {{Automatic}}},
         GridBoxSpacings->{"Columns" -> {
             Offset[0.27999999999999997`], {
              Offset[0.21]}, 
             Offset[0.27999999999999997`]}, "ColumnsIndexed" -> {}, "Rows" -> {
             Offset[0.2], {
              Offset[0.4]}, 
             Offset[0.2]}, "RowsIndexed" -> {}}], "  ", GridBox[{
          {
           RadioButtonBox[Dynamic[$CellContext`pol3], {"ppol"},
            DefaultBaseStyle->"RadioButtonBar"], 
           StyleBox["\<\"p\"\>", "RadioButtonBarLabel",
            StripOnInput->False]}
         },
         AutoDelete->False,
         GridBoxAlignment->{
          "Columns" -> {{Left}}, "ColumnsIndexed" -> {}, 
           "Rows" -> {{Baseline}}, "RowsIndexed" -> {}},
         GridBoxItemSize->{
          "Columns" -> {{Automatic}}, "Rows" -> {{Automatic}}},
         GridBoxSpacings->{"Columns" -> {
             Offset[0.27999999999999997`], {
              Offset[0.21]}, 
             Offset[0.27999999999999997`]}, "ColumnsIndexed" -> {}, "Rows" -> {
             Offset[0.2], {
              Offset[0.4]}, 
             Offset[0.2]}, "RowsIndexed" -> {}}]}], "Deploy"],
      RadioButtonBar[
       Dynamic[$CellContext`pol3], {"spol" -> "s", "ppol" -> "p"}]], 
     InterpretationBox[
      StyleBox[
       RowBox[{GridBox[{
          {
           RadioButtonBox[Dynamic[$CellContext`plane3], {"zplus"},
            DefaultBaseStyle->"RadioButtonBar"], 
           StyleBox["\<\"z>0\"\>", "RadioButtonBarLabel",
            StripOnInput->False]}
         },
         AutoDelete->False,
         GridBoxAlignment->{
          "Columns" -> {{Left}}, "ColumnsIndexed" -> {}, 
           "Rows" -> {{Baseline}}, "RowsIndexed" -> {}},
         GridBoxItemSize->{
          "Columns" -> {{Automatic}}, "Rows" -> {{Automatic}}},
         GridBoxSpacings->{"Columns" -> {
             Offset[0.27999999999999997`], {
              Offset[0.21]}, 
             Offset[0.27999999999999997`]}, "ColumnsIndexed" -> {}, "Rows" -> {
             Offset[0.2], {
              Offset[0.4]}, 
             Offset[0.2]}, "RowsIndexed" -> {}}], "  ", GridBox[{
          {
           RadioButtonBox[Dynamic[$CellContext`plane3], {"zmin"},
            DefaultBaseStyle->"RadioButtonBar"], 
           StyleBox["\<\"z<0\"\>", "RadioButtonBarLabel",
            StripOnInput->False]}
         },
         AutoDelete->False,
         GridBoxAlignment->{
          "Columns" -> {{Left}}, "ColumnsIndexed" -> {}, 
           "Rows" -> {{Baseline}}, "RowsIndexed" -> {}},
         GridBoxItemSize->{
          "Columns" -> {{Automatic}}, "Rows" -> {{Automatic}}},
         GridBoxSpacings->{"Columns" -> {
             Offset[0.27999999999999997`], {
              Offset[0.21]}, 
             Offset[0.27999999999999997`]}, "ColumnsIndexed" -> {}, "Rows" -> {
             Offset[0.2], {
              Offset[0.4]}, 
             Offset[0.2]}, "RowsIndexed" -> {}}]}], "Deploy"],
      RadioButtonBar[
       Dynamic[$CellContext`plane3], {"zplus" -> "z>0", "zmin" -> "z<0"}]], 
     InputFieldBox[Dynamic[$CellContext`reA3, SaveDefinitions -> True], Number,
      FieldHint->"Re[A3]",
      FieldSize->Tiny], "\<\"+\[ImaginaryI]\"\>", 
     InputFieldBox[Dynamic[$CellContext`imA3, SaveDefinitions -> True], Number,
      FieldHint->"Im[A3]",
      FieldSize->Tiny]},
    {
     CheckboxBox[Dynamic[$CellContext`point4], {True, False}], 
     StyleBox["\<\"4th point\"\>",
      StripOnInput->False,
      FontFamily->"Helvetica"], 
     InputFieldBox[Dynamic[$CellContext`kx4], Number,
      FieldHint->"kx4/k0",
      FieldSize->Tiny], 
     InputFieldBox[Dynamic[$CellContext`ky4], Number,
      FieldHint->"ky4/k0",
      FieldSize->Tiny], 
     InterpretationBox[
      StyleBox[
       RowBox[{GridBox[{
          {
           RadioButtonBox[Dynamic[$CellContext`pol4], {"spol"},
            DefaultBaseStyle->"RadioButtonBar"], 
           StyleBox["\<\"s\"\>", "RadioButtonBarLabel",
            StripOnInput->False]}
         },
         AutoDelete->False,
         GridBoxAlignment->{
          "Columns" -> {{Left}}, "ColumnsIndexed" -> {}, 
           "Rows" -> {{Baseline}}, "RowsIndexed" -> {}},
         GridBoxItemSize->{
          "Columns" -> {{Automatic}}, "Rows" -> {{Automatic}}},
         GridBoxSpacings->{"Columns" -> {
             Offset[0.27999999999999997`], {
              Offset[0.21]}, 
             Offset[0.27999999999999997`]}, "ColumnsIndexed" -> {}, "Rows" -> {
             Offset[0.2], {
              Offset[0.4]}, 
             Offset[0.2]}, "RowsIndexed" -> {}}], "  ", GridBox[{
          {
           RadioButtonBox[Dynamic[$CellContext`pol4], {"ppol"},
            DefaultBaseStyle->"RadioButtonBar"], 
           StyleBox["\<\"p\"\>", "RadioButtonBarLabel",
            StripOnInput->False]}
         },
         AutoDelete->False,
         GridBoxAlignment->{
          "Columns" -> {{Left}}, "ColumnsIndexed" -> {}, 
           "Rows" -> {{Baseline}}, "RowsIndexed" -> {}},
         GridBoxItemSize->{
          "Columns" -> {{Automatic}}, "Rows" -> {{Automatic}}},
         GridBoxSpacings->{"Columns" -> {
             Offset[0.27999999999999997`], {
              Offset[0.21]}, 
             Offset[0.27999999999999997`]}, "ColumnsIndexed" -> {}, "Rows" -> {
             Offset[0.2], {
              Offset[0.4]}, 
             Offset[0.2]}, "RowsIndexed" -> {}}]}], "Deploy"],
      RadioButtonBar[
       Dynamic[$CellContext`pol4], {"spol" -> "s", "ppol" -> "p"}]], 
     InterpretationBox[
      StyleBox[
       RowBox[{GridBox[{
          {
           RadioButtonBox[Dynamic[$CellContext`plane4], {"zplus"},
            DefaultBaseStyle->"RadioButtonBar"], 
           StyleBox["\<\"z>0\"\>", "RadioButtonBarLabel",
            StripOnInput->False]}
         },
         AutoDelete->False,
         GridBoxAlignment->{
          "Columns" -> {{Left}}, "ColumnsIndexed" -> {}, 
           "Rows" -> {{Baseline}}, "RowsIndexed" -> {}},
         GridBoxItemSize->{
          "Columns" -> {{Automatic}}, "Rows" -> {{Automatic}}},
         GridBoxSpacings->{"Columns" -> {
             Offset[0.27999999999999997`], {
              Offset[0.21]}, 
             Offset[0.27999999999999997`]}, "ColumnsIndexed" -> {}, "Rows" -> {
             Offset[0.2], {
              Offset[0.4]}, 
             Offset[0.2]}, "RowsIndexed" -> {}}], "  ", GridBox[{
          {
           RadioButtonBox[Dynamic[$CellContext`plane4], {"zmin"},
            DefaultBaseStyle->"RadioButtonBar"], 
           StyleBox["\<\"z<0\"\>", "RadioButtonBarLabel",
            StripOnInput->False]}
         },
         AutoDelete->False,
         GridBoxAlignment->{
          "Columns" -> {{Left}}, "ColumnsIndexed" -> {}, 
           "Rows" -> {{Baseline}}, "RowsIndexed" -> {}},
         GridBoxItemSize->{
          "Columns" -> {{Automatic}}, "Rows" -> {{Automatic}}},
         GridBoxSpacings->{"Columns" -> {
             Offset[0.27999999999999997`], {
              Offset[0.21]}, 
             Offset[0.27999999999999997`]}, "ColumnsIndexed" -> {}, "Rows" -> {
             Offset[0.2], {
              Offset[0.4]}, 
             Offset[0.2]}, "RowsIndexed" -> {}}]}], "Deploy"],
      RadioButtonBar[
       Dynamic[$CellContext`plane4], {"zplus" -> "z>0", "zmin" -> "z<0"}]], 
     InputFieldBox[Dynamic[$CellContext`reA4, SaveDefinitions -> True], Number,
      FieldHint->"Re[A4]",
      FieldSize->Tiny], "\<\"+\[ImaginaryI]\"\>", 
     InputFieldBox[Dynamic[$CellContext`imA4, SaveDefinitions -> True], Number,
      FieldHint->"Im[A4]",
      FieldSize->Tiny]},
    {
     CheckboxBox[Dynamic[$CellContext`point5], {True, False}], 
     StyleBox["\<\"5th point\"\>",
      StripOnInput->False,
      FontFamily->"Helvetica"], 
     InputFieldBox[Dynamic[$CellContext`kx5], Number,
      FieldHint->"kx5/k0",
      FieldSize->Tiny], 
     InputFieldBox[Dynamic[$CellContext`ky5], Number,
      FieldHint->"ky5/k0",
      FieldSize->Tiny], 
     InterpretationBox[
      StyleBox[
       RowBox[{GridBox[{
          {
           RadioButtonBox[Dynamic[$CellContext`pol5], {"spol"},
            DefaultBaseStyle->"RadioButtonBar"], 
           StyleBox["\<\"s\"\>", "RadioButtonBarLabel",
            StripOnInput->False]}
         },
         AutoDelete->False,
         GridBoxAlignment->{
          "Columns" -> {{Left}}, "ColumnsIndexed" -> {}, 
           "Rows" -> {{Baseline}}, "RowsIndexed" -> {}},
         GridBoxItemSize->{
          "Columns" -> {{Automatic}}, "Rows" -> {{Automatic}}},
         GridBoxSpacings->{"Columns" -> {
             Offset[0.27999999999999997`], {
              Offset[0.21]}, 
             Offset[0.27999999999999997`]}, "ColumnsIndexed" -> {}, "Rows" -> {
             Offset[0.2], {
              Offset[0.4]}, 
             Offset[0.2]}, "RowsIndexed" -> {}}], "  ", GridBox[{
          {
           RadioButtonBox[Dynamic[$CellContext`pol5], {"ppol"},
            DefaultBaseStyle->"RadioButtonBar"], 
           StyleBox["\<\"p\"\>", "RadioButtonBarLabel",
            StripOnInput->False]}
         },
         AutoDelete->False,
         GridBoxAlignment->{
          "Columns" -> {{Left}}, "ColumnsIndexed" -> {}, 
           "Rows" -> {{Baseline}}, "RowsIndexed" -> {}},
         GridBoxItemSize->{
          "Columns" -> {{Automatic}}, "Rows" -> {{Automatic}}},
         GridBoxSpacings->{"Columns" -> {
             Offset[0.27999999999999997`], {
              Offset[0.21]}, 
             Offset[0.27999999999999997`]}, "ColumnsIndexed" -> {}, "Rows" -> {
             Offset[0.2], {
              Offset[0.4]}, 
             Offset[0.2]}, "RowsIndexed" -> {}}]}], "Deploy"],
      RadioButtonBar[
       Dynamic[$CellContext`pol5], {"spol" -> "s", "ppol" -> "p"}]], 
     InterpretationBox[
      StyleBox[
       RowBox[{GridBox[{
          {
           RadioButtonBox[Dynamic[$CellContext`plane5], {"zplus"},
            DefaultBaseStyle->"RadioButtonBar"], 
           StyleBox["\<\"z>0\"\>", "RadioButtonBarLabel",
            StripOnInput->False]}
         },
         AutoDelete->False,
         GridBoxAlignment->{
          "Columns" -> {{Left}}, "ColumnsIndexed" -> {}, 
           "Rows" -> {{Baseline}}, "RowsIndexed" -> {}},
         GridBoxItemSize->{
          "Columns" -> {{Automatic}}, "Rows" -> {{Automatic}}},
         GridBoxSpacings->{"Columns" -> {
             Offset[0.27999999999999997`], {
              Offset[0.21]}, 
             Offset[0.27999999999999997`]}, "ColumnsIndexed" -> {}, "Rows" -> {
             Offset[0.2], {
              Offset[0.4]}, 
             Offset[0.2]}, "RowsIndexed" -> {}}], "  ", GridBox[{
          {
           RadioButtonBox[Dynamic[$CellContext`plane5], {"zmin"},
            DefaultBaseStyle->"RadioButtonBar"], 
           StyleBox["\<\"z<0\"\>", "RadioButtonBarLabel",
            StripOnInput->False]}
         },
         AutoDelete->False,
         GridBoxAlignment->{
          "Columns" -> {{Left}}, "ColumnsIndexed" -> {}, 
           "Rows" -> {{Baseline}}, "RowsIndexed" -> {}},
         GridBoxItemSize->{
          "Columns" -> {{Automatic}}, "Rows" -> {{Automatic}}},
         GridBoxSpacings->{"Columns" -> {
             Offset[0.27999999999999997`], {
              Offset[0.21]}, 
             Offset[0.27999999999999997`]}, "ColumnsIndexed" -> {}, "Rows" -> {
             Offset[0.2], {
              Offset[0.4]}, 
             Offset[0.2]}, "RowsIndexed" -> {}}]}], "Deploy"],
      RadioButtonBar[
       Dynamic[$CellContext`plane5], {"zplus" -> "z>0", "zmin" -> "z<0"}]], 
     InputFieldBox[Dynamic[$CellContext`reA5, SaveDefinitions -> True], Number,
      FieldHint->"Re[A5]",
      FieldSize->Tiny], "\<\"+\[ImaginaryI]\"\>", 
     InputFieldBox[Dynamic[$CellContext`imA5, SaveDefinitions -> True], Number,
      FieldHint->"Im[A5]",
      FieldSize->Tiny]},
    {
     CheckboxBox[Dynamic[$CellContext`point6], {True, False}], 
     StyleBox["\<\"6th point\"\>",
      StripOnInput->False,
      FontFamily->"Helvetica"], 
     InputFieldBox[Dynamic[$CellContext`kx6], Number,
      FieldHint->"kx6/k0",
      FieldSize->Tiny], 
     InputFieldBox[Dynamic[$CellContext`ky6], Number,
      FieldHint->"ky6/k0",
      FieldSize->Tiny], 
     InterpretationBox[
      StyleBox[
       RowBox[{GridBox[{
          {
           RadioButtonBox[Dynamic[$CellContext`pol6], {"spol"},
            DefaultBaseStyle->"RadioButtonBar"], 
           StyleBox["\<\"s\"\>", "RadioButtonBarLabel",
            StripOnInput->False]}
         },
         AutoDelete->False,
         GridBoxAlignment->{
          "Columns" -> {{Left}}, "ColumnsIndexed" -> {}, 
           "Rows" -> {{Baseline}}, "RowsIndexed" -> {}},
         GridBoxItemSize->{
          "Columns" -> {{Automatic}}, "Rows" -> {{Automatic}}},
         GridBoxSpacings->{"Columns" -> {
             Offset[0.27999999999999997`], {
              Offset[0.21]}, 
             Offset[0.27999999999999997`]}, "ColumnsIndexed" -> {}, "Rows" -> {
             Offset[0.2], {
              Offset[0.4]}, 
             Offset[0.2]}, "RowsIndexed" -> {}}], "  ", GridBox[{
          {
           RadioButtonBox[Dynamic[$CellContext`pol6], {"ppol"},
            DefaultBaseStyle->"RadioButtonBar"], 
           StyleBox["\<\"p\"\>", "RadioButtonBarLabel",
            StripOnInput->False]}
         },
         AutoDelete->False,
         GridBoxAlignment->{
          "Columns" -> {{Left}}, "ColumnsIndexed" -> {}, 
           "Rows" -> {{Baseline}}, "RowsIndexed" -> {}},
         GridBoxItemSize->{
          "Columns" -> {{Automatic}}, "Rows" -> {{Automatic}}},
         GridBoxSpacings->{"Columns" -> {
             Offset[0.27999999999999997`], {
              Offset[0.21]}, 
             Offset[0.27999999999999997`]}, "ColumnsIndexed" -> {}, "Rows" -> {
             Offset[0.2], {
              Offset[0.4]}, 
             Offset[0.2]}, "RowsIndexed" -> {}}]}], "Deploy"],
      RadioButtonBar[
       Dynamic[$CellContext`pol6], {"spol" -> "s", "ppol" -> "p"}]], 
     InterpretationBox[
      StyleBox[
       RowBox[{GridBox[{
          {
           RadioButtonBox[Dynamic[$CellContext`plane6], {"zplus"},
            DefaultBaseStyle->"RadioButtonBar"], 
           StyleBox["\<\"z>0\"\>", "RadioButtonBarLabel",
            StripOnInput->False]}
         },
         AutoDelete->False,
         GridBoxAlignment->{
          "Columns" -> {{Left}}, "ColumnsIndexed" -> {}, 
           "Rows" -> {{Baseline}}, "RowsIndexed" -> {}},
         GridBoxItemSize->{
          "Columns" -> {{Automatic}}, "Rows" -> {{Automatic}}},
         GridBoxSpacings->{"Columns" -> {
             Offset[0.27999999999999997`], {
              Offset[0.21]}, 
             Offset[0.27999999999999997`]}, "ColumnsIndexed" -> {}, "Rows" -> {
             Offset[0.2], {
              Offset[0.4]}, 
             Offset[0.2]}, "RowsIndexed" -> {}}], "  ", GridBox[{
          {
           RadioButtonBox[Dynamic[$CellContext`plane6], {"zmin"},
            DefaultBaseStyle->"RadioButtonBar"], 
           StyleBox["\<\"z<0\"\>", "RadioButtonBarLabel",
            StripOnInput->False]}
         },
         AutoDelete->False,
         GridBoxAlignment->{
          "Columns" -> {{Left}}, "ColumnsIndexed" -> {}, 
           "Rows" -> {{Baseline}}, "RowsIndexed" -> {}},
         GridBoxItemSize->{
          "Columns" -> {{Automatic}}, "Rows" -> {{Automatic}}},
         GridBoxSpacings->{"Columns" -> {
             Offset[0.27999999999999997`], {
              Offset[0.21]}, 
             Offset[0.27999999999999997`]}, "ColumnsIndexed" -> {}, "Rows" -> {
             Offset[0.2], {
              Offset[0.4]}, 
             Offset[0.2]}, "RowsIndexed" -> {}}]}], "Deploy"],
      RadioButtonBar[
       Dynamic[$CellContext`plane6], {"zplus" -> "z>0", "zmin" -> "z<0"}]], 
     InputFieldBox[Dynamic[$CellContext`reA6, SaveDefinitions -> True], Number,
      FieldHint->"Re[A6]",
      FieldSize->Tiny], "\<\"+\[ImaginaryI]\"\>", 
     InputFieldBox[Dynamic[$CellContext`imA6, SaveDefinitions -> True], Number,
      FieldHint->"Im[A6]",
      FieldSize->Tiny]}
   },
   AutoDelete->False,
   GridBoxFrame->{"Columns" -> {{True}}, "Rows" -> {{True}}},
   GridBoxItemSize->{"Columns" -> {{Automatic}}, "Rows" -> {{Automatic}}}],
  "Grid"]], "Output"],

Cell[BoxData[
 InterpretationBox[
  RowBox[{
   StyleBox[
    SubscriptBox["k", "0"],
    StripOnInput->False,
    FontFamily->"Helvetica",
    FontSize->14], "\[InvisibleSpace]", 
   StyleBox[
    TagBox[
     RowBox[{"(", "\[NoBreak]", 
      TagBox[GridBox[{
         {"\<\"\\!\\(\\*SubscriptBox[\\(p\\), \\(x\\)]\\)\"\>"},
         {"\<\"\\!\\(\\*SubscriptBox[\\(p\\), \\(y\\)]\\)\"\>"},
         {"\<\"\\!\\(\\*SubscriptBox[\\(p\\), \\(z\\)]\\)\"\>"},
         {"\<\"\\!\\(\\*SubscriptBox[\\(m\\), \\(x\\)]\\)\"\>"},
         {"\<\"\\!\\(\\*SubscriptBox[\\(m\\), \\(y\\)]\\)\"\>"},
         {"\<\"\\!\\(\\*SubscriptBox[\\(m\\), \\(z\\)]\\)\"\>"}
        },
        GridBoxAlignment->{
         "Columns" -> {{Center}}, "ColumnsIndexed" -> {}, 
          "Rows" -> {{Baseline}}, "RowsIndexed" -> {}},
        GridBoxSpacings->{"Columns" -> {
            Offset[0.27999999999999997`], {
             Offset[0.5599999999999999]}, 
            Offset[0.27999999999999997`]}, "ColumnsIndexed" -> {}, "Rows" -> {
            Offset[0.2], {
             Offset[0.4]}, 
            Offset[0.2]}, "RowsIndexed" -> {}}],
       Column], "\[NoBreak]", ")"}],
     Function[BoxForm`e$, 
      MatrixForm[BoxForm`e$]]],
    StripOnInput->False,
    FontFamily->"Helvetica",
    FontSize->14], "\[InvisibleSpace]", 
   StyleBox["\<\"=\"\>",
    StripOnInput->False,
    FontFamily->"Helvetica",
    FontSize->14], "\[InvisibleSpace]", 
   StyleBox[
    DynamicBox[ToBoxes[
      MatrixForm[
       Simplify[$CellContext`sol]], StandardForm],
     ImageSizeCache->{24., {50., 58.}}],
    StripOnInput->False,
    FontFamily->"Helvetica",
    FontSize->14], "\[InvisibleSpace]", 
   StyleBox[
    DynamicBox[ToBoxes[$CellContext`plus, StandardForm],
     ImageSizeCache->{8., {0., 8.}}],
    StripOnInput->False,
    FontFamily->"Helvetica",
    FontSize->14], "\[InvisibleSpace]", 
   StyleBox[
    DynamicBox[ToBoxes[
      Simplify[$CellContext`ToPrint], StandardForm],
     ImageSizeCache->{460., {62., 70.}}],
    StripOnInput->False,
    FontFamily->"Helvetica",
    FontSize->14], "\[InvisibleSpace]", 
   StyleBox["\<\" \"\>",
    StripOnInput->False,
    FontFamily->"Helvetica",
    FontSize->64], "\[InvisibleSpace]", 
   StyleBox[
    DynamicBox[ToBoxes[$CellContext`with, StandardForm],
     ImageSizeCache->{28., {0., 11.}}],
    StripOnInput->False,
    FontFamily->"Helvetica",
    FontSize->14], "\[InvisibleSpace]", 
   StyleBox["\<\" \"\>",
    StripOnInput->False,
    FontFamily->"Helvetica",
    FontSize->14], "\[InvisibleSpace]", 
   StyleBox[
    DynamicBox[ToBoxes[$CellContext`coef, StandardForm],
     ImageSizeCache->{87., {3., 11.}}],
    StripOnInput->False,
    FontFamily->"Helvetica",
    FontSize->14], "\[InvisibleSpace]", 
   StyleBox["\<\" \"\>",
    StripOnInput->False,
    FontFamily->"Helvetica",
    FontSize->14], "\[InvisibleSpace]", 
   StyleBox[
    DynamicBox[ToBoxes[$CellContext`compl, StandardForm],
     ImageSizeCache->{18., {0., 11.}}],
    StripOnInput->False,
    FontFamily->"Helvetica",
    FontSize->14]}],
  SequenceForm[
   Style[
    Subscript[$CellContext`k, 0], FontFamily -> "Helvetica", FontSize -> 14], 
   Style[
    MatrixForm[{
     "\!\(\*SubscriptBox[\(p\), \(x\)]\)", 
      "\!\(\*SubscriptBox[\(p\), \(y\)]\)", 
      "\!\(\*SubscriptBox[\(p\), \(z\)]\)", 
      "\!\(\*SubscriptBox[\(m\), \(x\)]\)", 
      "\!\(\*SubscriptBox[\(m\), \(y\)]\)", 
      "\!\(\*SubscriptBox[\(m\), \(z\)]\)"}], FontFamily -> "Helvetica", 
    FontSize -> 14], 
   Style["=", FontFamily -> "Helvetica", FontSize -> 14], 
   Style[
    Dynamic[
     MatrixForm[
      Simplify[$CellContext`sol]], SaveDefinitions -> True], FontFamily -> 
    "Helvetica", FontSize -> 14], 
   Style[
    Dynamic[$CellContext`plus, SaveDefinitions -> True], FontFamily -> 
    "Helvetica", FontSize -> 14], 
   Style[
    Dynamic[
     Simplify[$CellContext`ToPrint], SaveDefinitions -> True], FontFamily -> 
    "Helvetica", FontSize -> 14], 
   Style[" ", FontFamily -> "Helvetica", FontSize -> 64], 
   Style[
    Dynamic[$CellContext`with, SaveDefinitions -> True], FontFamily -> 
    "Helvetica", FontSize -> 14], 
   Style[" ", FontFamily -> "Helvetica", FontSize -> 14], 
   Style[
    Dynamic[$CellContext`coef, SaveDefinitions -> True], FontFamily -> 
    "Helvetica", FontSize -> 14], 
   Style[" ", FontFamily -> "Helvetica", FontSize -> 14], 
   Style[
    Dynamic[$CellContext`compl, SaveDefinitions -> True], FontFamily -> 
    "Helvetica", FontSize -> 14]],
  Editable->False]], "Print"]
}, Open  ]]
},
WindowSize->{1904, 1117},
WindowMargins->{{-8, Automatic}, {Automatic, 0}},
Visible->True,
ScrollingOptions->{"VerticalScrollRange"->Fit},
ShowCellBracket->Automatic,
CellContext->Notebook,
TrackCellChangeTimes->False,
FrontEndVersion->"11.0 for Microsoft Windows (64-bit) (July 28, 2016)",
StyleDefinitions->"Default.nb"
]
(* End of Notebook Content *)

(* Internal cache information *)
(*CellTagsOutline
CellTagsIndex->{}
*)
(*CellTagsIndex
CellTagsIndex->{}
*)
(*NotebookFileOutline
Notebook[{
Cell[CellGroupData[{
Cell[1486, 35, 56491, 1417, 19, "Input",
 CellOpen->False],
Cell[57980, 1454, 3845, 65, 19, "Output",
 CellOpen->False],
Cell[61828, 1521, 3840, 65, 19, "Output",
 CellOpen->False],
Cell[65671, 1588, 4569, 80, 19, "Output",
 CellOpen->False],
Cell[70243, 1670, 4597, 81, 19, "Output",
 CellOpen->False],
Cell[74843, 1753, 4597, 81, 19, "Output",
 CellOpen->False],
Cell[79443, 1836, 4599, 83, 19, "Output",
 CellOpen->False],
Cell[84045, 1921, 675, 15, 19, "Output",
 CellOpen->False],
Cell[84723, 1938, 282, 7, 19, "Output",
 CellOpen->False],
Cell[85008, 1947, 3783, 84, 19, "Output",
 CellOpen->False],
Cell[88794, 2033, 1304, 23, 105, "Output"],
Cell[90101, 2058, 26697, 653, 204, "Output"],
Cell[116801, 2713, 4528, 127, 142, "Print"]
}, Open  ]]
}
]
*)

(* NotebookSignature kuDNzySus6vvrD1abNJyNnWv *)
