@Comment(This is a special version of manual.mak to make the Global
	Index for the Garnet manual)

@Comment[ Copyright (C) 1979, 1987  Scribe Systems Inc.
           Change log: see last page of file  ]

@Marker(Make,GManual,Press,X9700,ImPress,Impress300,Impress480,X2700,NonScaleableLaser,Quic,Impress12-300)

@Define(BodyStyle,Font BodyFont,Spacing 1.5,Spread 0.8)
@Define(NoteStyle,Font SmallBodyFont,FaceCode R,Spacing 1)
@LibraryFile(Serif)
@Style(DoubleSided,BindingMargin=0.3inch)

@Generate(Notes,Outline,Index,Contents)
@Send(Notes    "@PrefaceSection(Notes)")
@Send(Contents "@PrefaceSection(Table of Contents)")
@Send(Contents "@Set(Page=1)@Style(PageNumber <@i>)")
@Send(Contents "@define(foot,invisible)")
@Send(#Index "@define(foot,invisible)")
@Send(#Index        "@UnNumbered(Index)",
      #Index        "@Begin(IndexEnv)")
@SendEnd(#Index "@End(IndexEnv)")

@Define(HDX,Hyphenation off,LeftMargin 0,Indent 0,Fill,Spaces compact,
         Above 1,Below 1,Break,Need 4,Justification Off,FaceCode R)
@Define(Hd0,Use HdX,Font TitleFont5,Above 1in,Below 0.5in,PageBreak UntilOdd)
@Define(Hd1,Use HdX,Font TitleFont5,Above .5inch,PageBreak UntilOdd)
@Define(HD1A=HD1,Centered,Tabexport False,Afterentry "@tabclear()")
@Define(Hd2,Use HdX,Font TitleFont3,Above 0.4inch)
@Define(Hd3,Use HdX,Font TitleFont3,Above 0.4inch)
@Define(Hd4,Use HdX,Font TitleFont3,Above 0.3inch)
@Define(TcX,LeftMargin 5,Indent -5,RightMargin 5,Fill,Spaces compact,Need 4,
         Above 0,Spacing 1,Below 0,Break,Spread 0,Justification off,FaceCode R)
@Define(Tc0=TcX,Font TitleFont3)
@Define(Tc1=TcX,Font TitleFont1,Above 0.1inch,Below 0.1inch,Need 1inch)
@Define(Tc2=TcX,LeftMargin 8,Font TitleFont0)
@Define(Tc3=TcX,LeftMargin 12,Font TitleFont0)
@Define(Tc4=TcX,LeftMargin 16,Font TitleFont0)
@Counter(MajorPart,TitleEnv HD0,ContentsEnv tc0,Numbered [@I],
          IncrementedBy Use,Announced)
@Counter(Chapter,TitleEnv HD1,ContentsEnv tc1,Numbered [@1.],
          IncrementedBy Use,Referenced [@1],Announced)
@Counter(Appendix,TitleEnv HD1,ContentsEnv tc1,Numbered [@I.],
          ContentsForm
           "@Begin@ParmQuote(ContentsEnv)Appendix @parm(referenced). @~
           @rfstr(@parm(page))@parm(Title)@End@ParmQuote(ContentsEnv)",
          TitleForm
           "@Begin@ParmQuote(TitleEnv)@=Appendix @parm(referenced)@~
           @*@=@Parm(Title)@End@ParmQuote(TitleEnv)",
          IncrementedBy,Referenced [@I],Announced,Alias Chapter)
@Counter(UnNumbered,TitleEnv HD1,ContentsEnv tc1,Announced,Alias Chapter)
@Counter(Section,Within Chapter,TitleEnv HD2,ContentsEnv tc2,
          Numbered [@#@:.@1.],Referenced [@#@:.@1],IncrementedBy Use,Announced)
@Counter(AppendixSection,Within Appendix,TitleEnv HD2,ContentsEnv tc2,
          Numbered [@#@:.@1.],Referenced [@#@:.@1],IncrementedBy Use,Announced)
@Counter(SubSection,Within Section,TitleEnv HD3,ContentsEnv tc3,
          Numbered [@#@:.@1.],IncrementedBy Use,Referenced [@#@:.@1])
@Counter(AppendixSubsection,Within AppendixSection,TitleEnv Hd3,
          ContentsEnv Tc3,Numbered [@#@:.@1.],IncrementedBy Use,
          Referenced [@#@:.@1])
@Counter(Paragraph,Within SubSection,TitleEnv HD4,ContentsEnv tc4,
          Numbered [@#@:.@1.],Referenced [@#@:.@1],IncrementedBy Use)

@Counter(PrefaceSection,TitleEnv HD1A,Alias Chapter)
@Define(IndexEnv,Break,CRBreak,Fill,BlankLines Kept,Font SmallBodyFont,
         FaceCode R,Spread 0,Spacing 1,Spaces Kept,LeftMargin 18,Indent -8)

@Modify(Page,Numbered <@#@:-@1>,Referenced <@#@:-@1>)
@LibraryFile(Figures)
@LibraryFile(TitlePage)

@Modify(EquationCounter,Within Chapter)
@Modify(TheoremCounter,Within Chapter)
@Equate(Sec=Section,Subsec=SubSection,Chap=Chapter,Para=Paragraph,
         SubSubSec=Paragraph,SubSubSection=Paragraph,
         AppendixSec=AppendixSection,AppendixSubSec=AppendixSubSection)

@Begin(Text,Indent 1Quad,LeftMargin 1inch,TopMargin 1inch,BottomMargin 1inch,
        RightMargin 1in,Use BodyStyle,FaceCode R)
@Set(Page=0)

@PageHeading(Even,Left "@value(Page)")
@PageHeading(Odd,Right "@value(Page)")

@Marker(Make,GManual,ReGIS)
@FontFamily(GGfont)
@LibraryFile(GIGIcolors)
@Define(BodyStyle,Font BodyFont,Spacing 1.5,Spread 0.8)
@Define(NoteStyle,Font BodyFont,Size -1,FaceCode R,Spacing 1)

@Generate(Notes,Outline,Index,Contents)
@Send(Notes    "@PrefaceSection(Notes)")
@Style(SingleSided)
@Send(Contents "@PrefaceSection(Table of Contents)")
@Send(Contents "@Set(Page=1)@Style(PageNumber <@i>)")
@Send(Contents "@define(foot,invisible)")
@Send(#Index "@define(foot,invisible)")
@Send(#Index        "@UnNumbered(Index)",
      #Index        "@Begin(IndexEnv)")
@SendEnd(#Index "@End(IndexEnv)")

@Define(HDX,Hyphenation off,LeftMargin 0,Indent 0,Fill,Spaces compact,Above 1,
         Below 0,Break,Need 4,Justification Off,Use B,Font BodyFont)
@Define(Hd0,Use HdX,Size +2,Above 1inch,Below 0.5inch,Color HD0color,
         PageBreak UntilOdd)
@Define(Hd1,Use HdX,Size +2,Above .5inch,Below .25inch,PageBreak UntilOdd,
         Color HD1color)
@Define(HD1A=HD1,Centered,Tabexport False,Afterentry "@tabclear()")
@Define(Hd2,Use HdX,Size +1,Above 0.4inch,Below 0.2inch,Color HD2color)
@Define(Hd3,Use HdX,Size +1,Above 0.4inch,Below 0.2inch,Color HD3color)
@Define(Hd4,Use HdX,Size +1,Above 0.3inch,Below 0.15inch,Color HD4color)

@Define(TcX,Indent -5,RightMargin 5,Fill,Spaces compact,Need 4,
         Above 0,Spacing 1,Below 0,Break,Spread 0,Justification off,
         Font BodyFont)
@Define(Tc0=TcX,Use B,LeftMargin 0,Size +1,Color TC0color)
@Define(Tc1=TcX,Use B,LeftMargin 4,Above 0.1in,Below 0.1inch,Need 1in,
         Color TC1color)
@Define(Tc2=TcX,LeftMargin 8,FaceCode R,Color TC2color)
@Define(Tc3=TcX,LeftMargin 12,FaceCode R,Color TC3color)
@Define(Tc4=TcX,LeftMargin 16,FaceCode R,Color TC4color)
@Counter(MajorPart,TitleEnv HD0,ContentsEnv tc0,Numbered [@I],
          IncrementedBy Use,Announced)
@Counter(Chapter,TitleEnv HD1,ContentsEnv tc1,Numbered [@1.],
          IncrementedBy Use,Referenced [@1],Announced)
@Counter(Appendix,TitleEnv HD1,ContentsEnv tc1,Numbered [@I.],
          ContentsForm
          "@Begin@ParmQuote(ContentsEnv)Appendix @parm(referenced). @~
          @rfstr(@parm(page))@parm(Title)@End@ParmQuote(ContentsEnv)",
          TitleForm
          "@Begin@ParmQuote(TitleEnv)@=Appendix @parm(referenced)@~
          @*@=@Parm(Title)@End@ParmQuote(TitleEnv)",
          IncrementedBy,Referenced [@I],Announced,Alias Chapter)
@Counter(UnNumbered,TitleEnv HD1,ContentsEnv tc1,Announced,Alias Chapter)
@Counter(Section,Within Chapter,TitleEnv HD2,ContentsEnv tc2,
          Numbered [@#@:.@1.],Referenced [@#@:.@1],IncrementedBy Use,Announced)
@Counter(AppendixSection,Within Appendix,TitleEnv HD2,ContentsEnv tc2,
          Numbered [@#@:.@1.],Referenced [@#@:.@1],IncrementedBy Use,Announced)
@Counter(SubSection,Within Section,TitleEnv HD3,ContentsEnv tc3,
          Numbered [@#@:.@1.],IncrementedBy Use,Referenced [@#@:.@1])
@Counter(AppendixSubSection,Within AppendixSection,TitleEnv Hd3,
          ContentsEnv Tc3,Numbered [@#@:.@1.],IncrementedBy Use,
          Referenced [@#@:.@1])
@Counter(Paragraph,Within SubSection,TitleEnv HD4,ContentsEnv tc4,
          Numbered [@#@:.@1.],Referenced [@#@:.@1],IncrementedBy Use)

@Counter(PrefaceSection,TitleEnv HD1A,Alias Chapter)
@Define(IndexEnv,Break,CRBreak,Fill,BlankLines Kept,Font BodyFont,Size 1,
         FaceCode R,Spread 0,Spacing 1,Spaces Kept,LeftMargin 18,Indent -8)

@Modify(Page,Numbered <@#@:-@1>,Referenced <@#@:-@1>)
@LibraryFile(Figures)
@LibraryFile(TitlePage)

@Modify(EquationCounter,Within Chapter)
@Modify(TheoremCounter,Within Chapter)

@Equate(Sec=Section,Subsec=SubSection,Chap=Chapter,Para=Paragraph,
         SubSubSec=Paragraph,SubSubSection=Paragraph,
         AppendixSec=AppendixSection,AppendixSubSec=AppendixSubSection)
@Begin(Text,Use BodyStyle,Indent 1Quad,Spread 0.075inch, Size 1,FaceCode R,
        LeftMargin 0.5in,RightMargin 0.5in,TopMargin 1in,BottomMargin .6in)

@Set(Page=0)


@PageHeading(Right "@value(Page)")

@Marker(Make,GManual,RobotTypewriter)
@Define(BodyStyle,Spacing 1.7,Spread 0.8)
@Define(TitleStyle,Spacing 1,Spread 0)
@Define(NoteStyle,Spacing 1,Spread 0.3)
@FontFamily(Elite 12)
@Style(DoubleSided,BindingMargin=0.3inch)

@Generate(Notes,Outline,Index,Contents)
@Send(Notes    "@PrefaceSection(Notes)")
@Send(Contents "@PrefaceSection(Table of Contents)")
@Send(Contents "@Set(Page=1)@Style(PageNumber <@i>)")
@Send(Contents "@define(foot,invisible)")
@Send(#Index "@define(foot,invisible)")
@Send(#Index        "@UnNumbered(Index)",
      #Index        "@Begin(IndexEnv)")
@SendEnd(#Index "@End(IndexEnv)")

@Define(HDX,Hyphenation off,LeftMargin 0,Indent 0,Fill,Spaces compact,
         Above 1,Below 0,Break,Need 4,Justification Off)
@Define(Hd0,Use HdX,Above 1inch,Below 0.5inch,PageBreak UntilOdd)
@Define(Hd1,Use HdX,Above .5inch,PageBreak UntilOdd)
@Define(HD1A=HD1,Centered,Tabexport False,Afterentry "@tabclear()")
@Define(Hd2,Use HdX,Above 0.4inch)
@Define(Hd3,Use HdX,Above 0.4inch)
@Define(Hd4,Use HdX,Above 0.3inch)
@Define(TcX,LeftMargin 4,Indent -5,RightMargin 5,Fill,Spaces compact,
         Above 0,Spacing 1,Below 0,Break,Spread 0,Justification off)
@Define(Tc0=TcX)
@Define(Tc1=TcX,Above 0.2,Below 0.2,Need 1inch)
@Define(Tc2=TcX,LeftMargin 8)
@Define(Tc3=TcX,LeftMargin 12)
@Define(Tc4=TcX,LeftMargin 16)
@Counter(MajorPart,TitleEnv HD0,ContentsEnv tc0,Numbered [@I],
          IncrementedBy Use,Announced)
@Counter(Chapter,TitleEnv HD1,ContentsEnv tc1,Numbered [@1.],
          IncrementedBy Use,Referenced [@1],Announced)
@Counter(Appendix,TitleEnv HD1,ContentsEnv tc1,Numbered [@I.],
          IncrementedBy,Referenced [@I],Announced,Alias Chapter)
@Counter(UnNumbered,TitleEnv HD1,ContentsEnv tc1,Announced,Alias Chapter)
@Counter(Section,Within Chapter,TitleEnv HD2,ContentsEnv tc2,
          Numbered [@#@:.@1.],Referenced [@#@:.@1],IncrementedBy Use,Announced)
@Counter(AppendixSection,Within Appendix,TitleEnv HD2,ContentsEnv tc2,
          Numbered [@#@:.@1.],Referenced [@#@:.@1],IncrementedBy Use,Announced)
@Counter(SubSection,Within Section,TitleEnv HD3,ContentsEnv tc3,
          Numbered [@#@:.@1.],IncrementedBy Use,Referenced [@#@:.@1])
@Counter(AppendixSubSection,Within AppendixSection,TitleEnv Hd3,ContentsEnv Tc3,
          Numbered [@#@:.@1.],IncrementedBy USe,Referenced [@#@:.@1])
@Counter(Paragraph,Within SubSection,TitleEnv HD4,ContentsEnv tc4,
          Numbered [@#@:.@1.],Referenced [@#@:.@1],IncrementedBy Use)

@Counter(PrefaceSection,TitleEnv HD1A,Alias Chapter)
@Define(IndexEnv,Break,CRBreak,Fill,BlankLines Kept,
         Spread 0,Spacing 1,Spaces Kept,LeftMargin 18,Indent -8)

@Modify(Page,Numbered <@#@:-@1>,Referenced <@#@:-@1>)
@LibraryFile(Figures)
@LibraryFile(TitlePage)

@Modify(EquationCounter,Within Chapter)
@Modify(TheoremCounter,Within Chapter)

@Equate(Sec=Section,Subsec=SubSection,Chap=Chapter,Para=Paragraph,
         SubSubSec=Paragraph,SubSubSection=Paragraph,
         AppendixSec=AppendixSection,AppendixSubSec=AppendixSubSection)
@Begin(Text,Indent 3,Use BodyStyle,LeftMargin 1inch,TopMargin 1inch,
        BottomMargin 1inch,RightMargin 1inch,Font CharDef,FaceCode R)
@Set(page=0)

@PageHeading(Even,Left "@value(Page)")
@PageHeading(Odd,Right "@value(Page)")

@Marker(Make,GManual,Santec)
@Define(BodyStyle,Spacing 1.7,Spread 0.8)
@Define(TitleStyle,Spacing 1,Spread 0)
@Define(NoteStyle,Spacing 1,Spread 0.3)
@case{Draft,
      1=<@FontFamily(Courier12D)>,
      else= <@FontFamily(Courier12)>}
@Style(DoubleSided,BindingMargin=0.3inch)

@Generate(Notes,Outline,Index,Contents)
@Send(Notes    "@PrefaceSection(Notes)")
@Send(Contents "@PrefaceSection(Table of Contents)")
@Send(Contents "@Set(Page=1)@Style(PageNumber <@i>)")
@Send(Contents "@define(foot,invisible)")
@Send(#Index "@define(foot,invisible)")
@Send(#Index        "@UnNumbered(Index)",
      #Index        "@Begin(IndexEnv)")
@SendEnd(#Index "@End(IndexEnv)")

@Define(HDX,Hyphenation off,LeftMargin 0,Indent 0,Fill,Spaces compact,
         Above 1,Below 0,Break,Need 4,Justification Off)
@Define(Hd0,Use HdX,Above 1inch,Below 0.5inch,PageBreak UntilOdd)
@Define(Hd1,Use HdX,Above .5inch,PageBreak UntilOdd,Below 0.25inch,
         Font TitleFont1,FaceCode R,BoldFace,Capitalized)
@Define(HD1A=HD1,Centered,Tabexport False,Afterentry "@tabclear()")
@Define(Hd2,Use HdX,Above 0.4inch,below 0.20inch,Font TitleFont1,
         FaceCode R,BoldFace)
@Define(Hd3,Use HdX,Above 0.4inch)
@Define(Hd4,Use HdX,Above 0.3inch)
@Define(TcX,LeftMargin 4,Indent -5,RightMargin 5,Fill,Spaces compact,
          Above 0,Spacing 1,Below 0,Break,Spread 0,Justification off)
@Define(Tc0=TcX)
@Define(Tc1=TcX,Above 0.2,Below 0.2,Need 1inch)
@Define(Tc2=TcX,LeftMargin 8)
@Define(Tc3=TcX,LeftMargin 12)
@Define(Tc4=TcX,LeftMargin 16)
@Counter(MajorPart,TitleEnv HD0,ContentsEnv tc0,Numbered [@I],
          IncrementedBy Use,Announced)
@Counter(Chapter,TitleEnv HD1,ContentsEnv tc1,Numbered [@1.],
          IncrementedBy Use,Referenced [@1],Announced)
@Counter(Appendix,TitleEnv HD1,ContentsEnv tc1,Numbered [@I.],
          IncrementedBy,Referenced [@I],Announced,Alias Chapter)
@Counter(UnNumbered,TitleEnv HD1,ContentsEnv tc1,Announced,Alias Chapter)
@Counter(Section,Within Chapter,TitleEnv HD2,ContentsEnv tc2,
          Numbered [@#@:.@1.],Referenced [@#@:.@1],IncrementedBy Use,Announced)
@Counter(AppendixSection,Within Appendix,TitleEnv HD2,ContentsEnv tc2,
          Numbered [@#@:.@1.],Referenced [@#@:.@1],IncrementedBy Use,Announced)
@Counter(SubSection,Within Section,TitleEnv HD3,ContentsEnv tc3,
          Numbered [@#@:.@1.],IncrementedBy Use,Referenced [@#@:.@1])
@Counter(AppendixSubSection,Within AppendixSection,TitleEnv Hd3,ContentsEnv Tc3,
           Numbered [@#@:.@1.],IncrementedBy USe,Referenced [@#@:.@1])
@Counter(Paragraph,Within SubSection,TitleEnv HD4,ContentsEnv tc4,
          Numbered [@#@:.@1.],Referenced [@#@:.@1],IncrementedBy Use)

@Counter(PrefaceSection,TitleEnv HD1A,Alias Chapter)
@Define(IndexEnv,Break,CRBreak,Fill,BlankLines Kept,
         Spread 0,Spacing 1,Spaces Kept,LeftMargin 18,Indent -8)

@Modify(Page,Numbered <@#@:-@1>,Referenced <@#@:-@1>)
@LibraryFile(Figures)
@LibraryFile(TitlePage)

@Modify(EquationCounter,Within Chapter)
@Modify(TheoremCounter,Within Chapter)

@Equate(Sec=Section,Subsec=SubSection,Chap=Chapter,Para=Paragraph,
          SubSubSec=Paragraph,SubSubSection=Paragraph,
         AppendixSec=AppendixSection,AppendixSubSec=AppendixSubSection)
@Comment{Zubkoff: Fix for temporary Scribe margin bug.
          @Begin(Text,Indent 3,Use BodyStyle,LeftMargin 1inch,TopMargin 1inch,
            BottomMargin 1inch,LineWidth 6.5inches,Justification,
            Spaces Compact,Font CharDef,FaceCode R)
         }
@Begin(Text,Indent 3,Use BodyStyle,LeftMargin 0.875inch,TopMargin 0.75inch,
        BottomMargin 1inch,LineWidth 6.5in,Font CharDef,FaceCode R)
@Modify(HDG,fixed  0.25inch)
@Modify(FTG,fixed 10.25inch)
@Set(page=0)

@PageHeading(Even,Left "@value(Page)")
@PageHeading(Odd,Right "@value(Page)")

@Marker(Make,GManual,GP300)
@Define(BodyStyle,Font BodyFont,Spacing 1.5,Spread 0.8)
@Define(NoteStyle,Font SmallBodyFont,FaceCode R,Spacing 1)
@LibraryFile(Serif)
@comment{@FontFamily(Times Roman 10)}
@Style(DoubleSided,BindingMargin=0.3inch)

@Generate(Notes,Outline,Index,Contents)
@Send(Notes    "@PrefaceSection(Notes)")
@Send(Contents "@PrefaceSection(Table of Contents)")
@Send(Contents "@Set(Page=1)@Style(PageNumber <@i>)")
@Send(Contents "@define(foot,invisible)")
@Send(#Index "@define(foot,invisible)")
@Send(#Index        "@UnNumbered(Index)",
      #Index        "@Begin(IndexEnv)")
@SendEnd(#Index "@End(IndexEnv)")

@Define(HDX,Hyphenation off,LeftMargin 0,Indent 0,Fill,Spaces compact,
         Above 1,Below 0,Break,Need 4,Justification Off)
@Define(Hd0,Use HdX,Font TitleFont5,FaceCode R,
         Use B,Use C,Above 1inch,Below 0.5inch,PageBreak UntilOdd)
@Define(Hd1,Use HdX,Font TitleFont5,FaceCode R,
         Use B,Use C,Above .5inch,PageBreak UntilOdd)
@Define(HD1A=HD1,Centered,Use B,Use C,Tabexport False,Afterentry "@tabclear()")
@Define(Hd2,Use HdX,Font TitleFont3,FaceCode R,Use B,Above 0.4inch)
@Define(Hd3,Use HdX,Font TitleFont3,FaceCode R,Above 0.4inch)
@Define(Hd4,Use HdX,Font TitleFont3,FaceCode R,Above 0.3inch)
@Define(TcX,LeftMargin 5,Indent -5,RightMargin 5,Fill,Spaces compact,Need 4,
         Above 0,Spacing 1,Below 0,Break,Spread 0,Justification off)
@Define(Tc0=TcX,Font TitleFont3,FaceCode R,Use B)
@Define(Tc1=TcX,Font TitleFont1,FaceCode R,Use B,Above 0.1inch,
         Below 0.1inch,Need 1inch)
@Define(Tc2=TcX,LeftMargin 8,Font TitleFont0,FaceCode R)
@Define(Tc3=TcX,LeftMargin 12,Font TitleFont0,FaceCode R)
@Define(Tc4=TcX,LeftMargin 16,Font TitleFont0,FaceCode R)
@Counter(MajorPart,TitleEnv HD0,ContentsEnv tc0,Numbered [@I],
          IncrementedBy Use,Announced)
@Counter(Chapter,TitleEnv HD1,ContentsEnv tc1,Numbered [@1.],
          IncrementedBy Use,Referenced [@1],Announced)
@Counter(Appendix,TitleEnv HD1,ContentsEnv tc1,Numbered [@I.],
          ContentsForm
           "@Begin@ParmQuote(ContentsEnv)Appendix @parm(referenced). @~
        @rfstr(@parm(page))@parm(Title)@End@ParmQuote(ContentsEnv)",
          TitleForm
        "@Begin@ParmQuote(TitleEnv)@=Appendix @parm(referenced)@~
        @*@=@Parm(Title)@End@ParmQuote(TitleEnv)",
          IncrementedBy,Referenced [@I],Announced,Alias Chapter)
@Counter(UnNumbered,TitleEnv HD1,ContentsEnv tc1,Announced,Alias Chapter)
@Counter(Section,Within Chapter,TitleEnv HD2,ContentsEnv tc2,
          Numbered [@#@:.@1.],Referenced [@#@:.@1],IncrementedBy Use,Announced)
@Counter(AppendixSection,Within Appendix,TitleEnv HD2,ContentsEnv tc2,
          Numbered [@#@:.@1.],Referenced [@#@:.@1],IncrementedBy Use,Announced)
@Counter(SubSection,Within Section,TitleEnv HD3,ContentsEnv tc3,
          Numbered [@#@:.@1.],IncrementedBy Use,Referenced [@#@:.@1])
@Counter(AppendixSubSection,Within AppendixSection,TitleEnv Hd3,ContentsEnv Tc3,
          Numbered [@#@:.@1.],IncrementedBy USe,Referenced [@#@:.@1])
@Counter(Paragraph,Within SubSection,TitleEnv HD4,ContentsEnv tc4,
          Numbered [@#@:.@1.],Referenced [@#@:.@1],IncrementedBy Use)

@Counter(PrefaceSection,TitleEnv HD1A,Alias Chapter)
@Define(IndexEnv,Break,CRBreak,Fill,BlankLines Kept,Font SmallBodyFont,
         FaceCode R,Spread 0,Spacing 1,Spaces Kept,LeftMargin 18,Indent -8)

@Modify(Page,Numbered <@#@:-@1>,Referenced <@#@:-@1>)
@LibraryFile(Figures)
@LibraryFile(TitlePage)

@Modify(EquationCounter,Within Chapter)
@Modify(TheoremCounter,Within Chapter)

@Equate(Sec=Section,Subsec=SubSection,Chap=Chapter,Para=Paragraph,
         SubSubSec=Paragraph,SubSubSection=Paragraph,
         AppendixSec=AppendixSection,AppendixSubSec=AppendixSubSection)
@Begin(Text,Indent 1Quad,LeftMargin 1inch,TopMargin 1inch,BottomMargin 1inch,
        RightMargin 1in,Use BodyStyle,FaceCode R)
@Set(Page=0)

@PageHeading(Even,Left "@value(Page)")
@PageHeading(Odd,Right "@value(Page)")

@Marker(Make,GManual,OmniTech,ScaleableLaser)
@LibraryFile(SansSerif)

@Define(BodyStyle,Font BodyFont,Spacing 1.3,Spread 0.8,size 10,indent 2 )
@Define(NoteStyle,Font BodyFont,FaceCode R,Spacing 1,size 8)
@Style(DoubleSided,BindingMargin=0.3inch)

@Generate(Notes,Outline,Index,Contents)
@Send(Notes    "@PrefaceSection(Notes)")
@Send(Contents "@PrefaceSection(Table of Contents)")
@Send(Contents "@Set(Page=1)@Style(PageNumber <@i>)")
@Send(Contents "@define(foot,invisible)")
@Send(#Index "@define(foot,invisible)")
@Send(#Index        "@UnNumbered(Global Index)",
#Index {@PageHeading(odd, immediate, left "@ux[Global Index @>Page @value(page)]")},
#Index {@PageHeading(even, immediate, left "@ux[Page @value(page)@hsp(0.15 in)Garnet Manual @>Global Index]")},
#index {@b(Note:)},
#index {@begin(itemize)},
#index {slot names are indexed without any leading colon (e.g. },
#index {:fixed is indexed as Fixed).  },
#index {},
#index {Lisp symbols are indexed without leading package names (e.g. kr::*PRINT-NEW-INSTANCES* is indexed as *PRINT-NEW-INSTANCES*).},
#index {@end(itemize)},
#Index        "@Begin(IndexEnv)")
@SendEnd(#Index "@End(IndexEnv)")

@Define(HDX,Hyphenation off,LeftMargin 0,Indent 0,Fill,Spaces compact,
         Above 1,Below 0,Break,Need 4,Justification Off,Font TitleFont,
         FaceCode B)
@Define(Hd0,Use HdX,Size  20,Above 1inch,Below 0.5inch,PageBreak UntilOdd)
@Define(Hd1,Use HdX,Size 16,Above .5inch,PageBreak UntilOdd)
@Define(HD1A=HD1,Centered,Tabexport False,Afterentry "@tabclear()")
@Define(Hd2,Use HdX,Size 14,Above 0.4inch)
@Define(Hd3,Use HdX,Size 12,Above 0.4inch)
@Define(Hd4,Use HdX,Size 12,Above 0.3inch)
@Define(TcX,LeftMargin 5,Indent -5,RightMargin 5,Fill,Spaces compact,Need 4,
         Above 0,Spacing 1,Below 0,Break,Spread 0,Justification off,
         Font TitleFont,FaceCode B)
@Define(Tc0=TcX,Size 14)
@Define(Tc1=TcX,Size 12,Above 0.1inch,Below 0.1inch,Need 1inch)
@Define(Tc2=TcX,LeftMargin 8,size 10)
@Define(Tc3=TcX,LeftMargin 12,Size 10)
@Define(Tc4=TcX,LeftMargin 16,Size 10)
@Counter(MajorPart,TitleEnv HD0,ContentsEnv tc0,Numbered [@I],
          IncrementedBy Use,Announced)
@Counter(Chapter,TitleEnv HD1,ContentsEnv tc1,Numbered [@1.],
          IncrementedBy Use,Referenced [@1],Announced)
@Counter(Appendix,TitleEnv HD1,ContentsEnv tc1,Numbered [@I.],
          ContentsForm
           "@Begin@ParmQuote(ContentsEnv)Appendix @parm(referenced). @~
        @rfstr(@parm(page))@parm(Title)@End@ParmQuote(ContentsEnv)",
          TitleForm
        "@Begin@ParmQuote(TitleEnv)@=Appendix @parm(referenced)@~
        @*@=@Parm(Title)@End@ParmQuote(TitleEnv)",
          IncrementedBy,Referenced [@I],Announced,Alias Chapter)
@Counter(UnNumbered,TitleEnv HD1,ContentsEnv tc1,Announced,Alias Chapter)
@Counter(Section,Within Chapter,TitleEnv HD2,ContentsEnv tc2,
          Numbered [@#@:.@1.],Referenced [@#@:.@1],IncrementedBy Use,Announced)
@Counter(AppendixSection,Within Appendix,TitleEnv HD2,ContentsEnv tc2,
          Numbered [@#@:.@1.],Referenced [@#@:.@1],IncrementedBy Use,Announced)
@Counter(SubSection,Within Section,TitleEnv HD3,ContentsEnv tc3,
          Numbered [@#@:.@1.],IncrementedBy Use,Referenced [@#@:.@1])
@Counter(AppendixSubSection,Within AppendixSection,TitleEnv Hd3,ContentsEnv Tc3,
          Numbered [@#@:.@1.],IncrementedBy USe,Referenced [@#@:.@1])
@Counter(Paragraph,Within SubSection,TitleEnv HD4,ContentsEnv tc4,
          Numbered [@#@:.@1.],Referenced [@#@:.@1],IncrementedBy Use)

@Counter(PrefaceSection,TitleEnv HD1A,Alias Chapter)
@Define(IndexEnv,Break,CRBreak,Fill,BlankLines Kept,Font BodyFont,size 8,
          FaceCode R,Spread 0,Spacing 1,Spaces Kept,LeftMargin 18,Indent -8)

@Modify(Page,Numbered <@#@:-@1>,Referenced <@#@:-@1>)
@LibraryFile(Figures)
@LibraryFile(TitlePage)

@Modify(EquationCounter,Within Chapter)
@Modify(TheoremCounter,Within Chapter)

@Equate(Sec=Section,Subsec=SubSection,Chap=Chapter,Para=Paragraph,
         SubSubSec=Paragraph,SubSubSection=Paragraph,
         AppendixSec=AppendixSection,AppendixSubSec=AppendixSubSection)
@Begin(Text,LeftMargin 1inch,TopMargin 1inch,BottomMargin 1inch,
        RightMargin 1inch,Use BodyStyle,FaceCode R)
@Set(Page=0)

@PageHeading(Even,Left "@value(Page)")
@PageHeading(Odd,Right "@value(Page)")


@Marker(Make,GManual,Cg8600,VIP)
@LibraryFile(SansSerif)

@Define(BodyStyle,Font BodyFont,Spacing 1.3,Spread 0.8,size 10,indent 2 )
@Define(NoteStyle,Font BodyFont,FaceCode R,Spacing 1,size 8)
@Style(DoubleSided,BindingMargin=0.3inch)

@Generate(Notes,Outline,Index,Contents)
@Send(Notes    "@PrefaceSection(Notes)")
@Send(Contents "@PrefaceSection(Table of Contents)")
@Send(Contents "@Set(Page=1)@Style(PageNumber <@i>)")
@Send(Contents "@define(foot,invisible)")
@Send(#Index "@define(foot,invisible)")
@Send(#Index        "@UnNumbered(Index)",
      #Index        "@Begin(IndexEnv)")
@SendEnd(#Index "@End(IndexEnv)")

@Define(HDX,Hyphenation off,LeftMargin 0,Indent 0,Fill,Spaces compact,Above 1,
         Below 0,Break,Need 4,Justification Off,Font TitleFont, FaceCode R)
@Define(Hd0,Use HdX,Size  20,Above 1inch,Below 0.5inch,PageBreak UntilOdd)
@Define(Hd1,Use HdX,Size 16,Above .5inch,PageBreak UntilOdd)
@Define(HD1A=HD1,Centered,Tabexport False,Afterentry "@tabclear()")
@Define(Hd2,Use HdX,Size 14,Above 0.4inch)
@Define(Hd3,Use HdX,Size 12,Above 0.4inch)
@Define(Hd4,Use HdX,Size 12,Above 0.3inch)
@Define(TcX,LeftMargin 5,Indent -5,RightMargin 5,Fill,Spaces compact,Need 4,
         Above 0,Spacing 1,Below 0,Break,Spread 0,Justification off,
         Font TitleFont,FaceCode R)
@Define(Tc0=TcX,Size 14)
@Define(Tc1=TcX,Size 12,Above 0.1inch,Below 0.1inch,Need 1inch)
@Define(Tc2=TcX,LeftMargin 8,Size 10)
@Define(Tc3=TcX,LeftMargin 12,Size 10)
@Define(Tc4=TcX,LeftMargin 16,Size 10)
@Counter(MajorPart,TitleEnv HD0,ContentsEnv tc0,Numbered [@I],
          IncrementedBy Use,Announced)
@Counter(Chapter,TitleEnv HD1,ContentsEnv tc1,Numbered [@1.],
          IncrementedBy Use,Referenced [@1],Announced)
@Counter(Appendix,TitleEnv HD1,ContentsEnv tc1,Numbered [@I.],
          ContentsForm
           "@Begin@ParmQuote(ContentsEnv)Appendix @parm(referenced). @~
        @rfstr(@parm(page))@parm(Title)@End@ParmQuote(ContentsEnv)",
          TitleForm
        "@Begin@ParmQuote(TitleEnv)@=Appendix @parm(referenced)@~
        @*@=@Parm(Title)@End@ParmQuote(TitleEnv)",
          IncrementedBy,Referenced [@I],Announced,Alias Chapter)
@Counter(UnNumbered,TitleEnv HD1,ContentsEnv tc1,Announced,Alias Chapter)
@Counter(Section,Within Chapter,TitleEnv HD2,ContentsEnv tc2,
          Numbered [@#@:.@1.],Referenced [@#@:.@1],IncrementedBy Use,Announced)
@Counter(AppendixSection,Within Appendix,TitleEnv HD2,ContentsEnv tc2,
          Numbered [@#@:.@1.],Referenced [@#@:.@1],IncrementedBy Use,Announced)
@Counter(SubSection,Within Section,TitleEnv HD3,ContentsEnv tc3,
          Numbered [@#@:.@1.],IncrementedBy Use,Referenced [@#@:.@1])
@Counter(AppendixSubSection,Within AppendixSection,TitleEnv Hd3,ContentsEnv Tc3,
          Numbered [@#@:.@1.],IncrementedBy USe,Referenced [@#@:.@1])
@Counter(Paragraph,Within SubSection,TitleEnv HD4,ContentsEnv tc4,
          Numbered [@#@:.@1.],Referenced [@#@:.@1],IncrementedBy Use)

@Counter(PrefaceSection,TitleEnv HD1A,Alias Chapter)
@Define(IndexEnv,Break,CRBreak,Fill,BlankLines Kept,Font BodyFont,size 8,
         FaceCode R,Spread 0,Spacing 1,Spaces Kept,LeftMargin 18,Indent -8)

@Modify(Page,Numbered <@#@:-@1>,Referenced <@#@:-@1>)
@LibraryFile(Figures)
@LibraryFile(TitlePage)

@Modify(EquationCounter,Within Chapter)
@Modify(TheoremCounter,Within Chapter)

@Equate(Sec=Section,Subsec=SubSection,Chap=Chapter,Para=Paragraph,
         SubSubSec=Paragraph,SubSubSection=Paragraph,
         AppendixSec=AppendixSection,AppendixSubSec=AppendixSubSection)
@Begin(Text,LeftMargin 0.5inch,TopMargin 1inch,BottomMargin 1inch,
        LineWidth 6.5inches,Use BodyStyle,FaceCode R)
@Set(Page=0)

@PageHeading(Even,Left "@value(Page)")
@PageHeading(Odd,Right "@value(Page)")


@Marker(Make,GManual,File)
@Define(BodyStyle,Spacing 1)
@Define(TitleStyle,Spacing 1)
@Define(NoteStyle,Spacing 1)

@Generate(Notes,Outline,Index,Contents)
@Send(Notes    "@PrefaceSection(Notes)")
@Send(Contents "@PrefaceSection(Table of Contents)")
@Send(Contents "@Set(Page=1)@Style(PageNumber <@i>)")
@Send(Contents "@define(foot,invisible)")
@Send(#Index "@define(foot,invisible)")
@Send(#Index        "@UnNumbered(Index)",
      #Index        "@Begin(IndexEnv)")
@SendEnd(#Index "@End(IndexEnv)")

@Define(HDX,Hyphenation off,LeftMargin 0,Indent 0,Fill,Spaces compact,
         Above 1,Below 0,break,need 4,Justification Off)
@Define(Hd0,Use HdX,Above 1inch,Below 0.5inch,PageBreak UntilOdd)
@Define(Hd1,Use HdX,Above 3,PageBreak Before)
@Define(HD1A=HD1,Centered,Tabexport False,Afterentry "@tabclear()")
@Define(Hd2,Use HdX)
@Define(Hd3,Use HdX,Above 3)
@Define(Hd4,Use HdX,Above 2)
@Define(TcX,LeftMargin 4,Indent -5,RightMargin 5,Fill,Spaces compact,
         Above 0,Spacing 1,Below 0,Break,Spread 0,Justification off)
@Define(Tc0=TcX)
@Define(Tc1=TcX,Above 1,Below 1,Need 1inch)
@Define(Tc2=TcX,LeftMargin 8,Need 1inch)
@Define(Tc3=TcX,LeftMargin 12)
@Define(Tc4=TcX,LeftMargin 16)
@Counter(MajorPart,TitleEnv HD0,ContentsEnv tc0,Numbered [@I],
          IncrementedBy Use,Announced)
@Counter(Chapter,TitleEnv HD1,ContentsEnv tc1,Numbered [@1.],
          IncrementedBy Use,Referenced [@1],Announced)
@Counter(Appendix,TitleEnv HD1,ContentsEnv tc1,Numbered [@I.],
          IncrementedBy,Referenced [@I],Announced,Alias Chapter)
@Counter(UnNumbered,TitleEnv HD1,ContentsEnv tc1,Announced,Alias Chapter)
@Counter(Section,Within Chapter,TitleEnv HD2,ContentsEnv tc2,
          Numbered [@#@:.@1.],Referenced [@#@:.@1],IncrementedBy Use,Announced)
@Counter(AppendixSection,Within Appendix,TitleEnv HD2,ContentsEnv tc2,
          Numbered [@#@:.@1.],Referenced [@#@:.@1],IncrementedBy Use,Announced)
@Counter(SubSection,Within Section,TitleEnv HD3,ContentsEnv tc3,
          Numbered [@#@:.@1.],IncrementedBy Use,Referenced [@#@:.@1])
@Counter(AppendixSubSection,Within AppendixSection,TitleEnv Hd3,ContentsEnv Tc3,
          Numbered [@#@:.@1.],IncrementedBy USe,Referenced [@#@:.@1])
@Counter(Paragraph,Within SubSection,TitleEnv HD4,ContentsEnv tc4,
          Numbered [@#@:.@1.],Referenced [@#@:.@1],IncrementedBy Use)

@Counter(PrefaceSection,TitleEnv HD1A,Alias Chapter)
@Define(IndexEnv,Break,CRBreak,Fill,BlankLines Kept,
         Spread 0,Spacing 1,Spaces Kept,LeftMargin 18,Indent -8)

@Modify(Page,Numbered <@#@:-@1>,Referenced <@#@:-@1>)
@LibraryFile(Figures)
@LibraryFile(TitlePage)

@Modify(EquationCounter,Within Chapter)
@Modify(TheoremCounter,Within Chapter)

@Equate(Sec=Section,Subsec=SubSection,Chap=Chapter,Para=Paragraph,
         SubSubSec=Paragraph,SubSubSection=Paragraph,
         AppendixSec=AppendixSection,AppendixSubSec=AppendixSubSection)
@Begin(Text,Indent 2,Spread 1,Use BodyStyle,LineWidth 7.9inches,
        Font CharDef,FaceCode R)
@Set(page=0)

@PageHeading(Even,Left "@value(Page)")
@PageHeading(Odd,Right "@value(Page)")


@Marker(Make,GManual,GSI)
@FontFamily(CMU 4)
@Define(BodyStyle,Font BodyFont,Spacing 1.3,Spread 0.8,Size 10,Indent 2)
@Define(NoteStyle,Font BodyFont,FaceCode R,Spacing 1,Size 8,Indent 2)
@Style(DoubleSided,BindingMargin=0.3inch)

@Generate(Notes,Outline,Index,Contents)
@Send(Notes    "@PrefaceSection(Notes)")
@Send(Contents "@PrefaceSection(Table of Contents)")
@Send(Contents "@Set(Page=1)@Style(PageNumber <@i>)")
@Send(Contents "@define(foot,invisible)")
@Send(#Index "@define(foot,invisible)")
@Send(#Index        "@UnNumbered(Index)",
      #Index        "@Begin(IndexEnv)")
@SendEnd(#Index "@End(IndexEnv)")

@Define(HDX,Hyphenation off,LeftMargin 0,Indent 0,Fill,Spaces compact,
         Above 1,Below 0,break,Need 4,Justification Off,Font TitleFont,
         FaceCode B)
@Define(Hd0,Use HdX,Size +8,Above 0.6inch,PageBreak UntilOdd)
@Define(Hd1,Use HdX,Size +4,Above 0.5inch,PageBreak UntilOdd)
@Define(Hd1A=Hd1,Centered,Tabexport False,Afterentry "@tabclear()")
@Define(Hd2,Use HdX,Size +2,Above 0.4inch)
@Define(Hd3,Use HdX,Size +1,Above 0.4inch)
@Define(Hd4,Use HdX,Size +0,Above 0.3inch)
@Define(TcX,Indent -5,RightMargin 5,Fill,Spaces compact,Need 4,
         Above 0,Spacing 1,Below 0,Break,Spread 0,Justification off,
         Font TitleFont,FaceCode R)
@Define(Tc0=TcX,LeftMargin 0,Size +4)
@Define(Tc1=TcX,LeftMargin 4,Size +2)
@Define(Tc2=TcX,LeftMargin 8,Size +1)
@Define(Tc3=TcX,LeftMargin 12,Size +0)
@Define(Tc4=TcX,LeftMargin 16,Size +0)
@Counter(MajorPart,TitleEnv HD0,ContentsEnv Tc0,Numbered [@I],
          IncrementedBy Use, Announced)
@Counter(Chapter,TitleEnv HD1,ContentsEnv tc1,Numbered [@1.],
          IncrementedBy Use,Referenced [@1],Announced)
@Counter(Appendix,TitleEnv HD1,ContentsEnv tc1,Numbered [@I.],
         ContentsForm
          "@Begin@ParmQuote(ContentsEnv)Appendix @parm(referenced). @~
        @rfstr(@parm(page))@parm(Title)@End@ParmQuote(ContentsEnv)",
          TitleForm
        "@Begin@ParmQuote(TitleEnv)@=Appendix @parm(referenced)@~
        @*@=@Parm(Title)@End@ParmQuote(TitleEnv)",
          IncrementedBy,Referenced [@I],Announced,Alias Chapter)
@Counter(UnNumbered,TitleEnv HD1,ContentsEnv tc1,Announced,Alias Chapter)
@Counter(Section,Within Chapter,TitleEnv HD2,ContentsEnv tc2,
          Numbered [@#@:.@1.],Referenced [@#@:.@1],IncrementedBy Use,Announced)
@Counter(AppendixSection,Within Appendix,TitleEnv HD2,ContentsEnv tc2,
          Numbered [@#@:.@1.],Referenced [@#@:.@1],IncrementedBy Use,Announced)
@Counter(SubSection,Within Section,TitleEnv HD3,ContentsEnv tc3,
          Numbered [@#@:.@1.],IncrementedBy Use,Referenced [@#@:.@1])
@Counter(AppendixSubSection,Within AppendixSection,TitleEnv Hd3,ContentsEnv Tc3,
          Numbered [@#@:.@1.],IncrementedBy USe,Referenced [@#@:.@1])
@Counter(Paragraph,Within SubSection,TitleEnv HD4,ContentsEnv tc4,
          Numbered [@#@:.@1.],Referenced [@#@:.@1],IncrementedBy Use)
@Counter(PrefaceSection,TitleEnv HD1A,Alias Chapter)
@Define(IndexEnv,Break,CRBreak,Fill,BlankLines Kept,Font BodyFont,Size 8,
         FaceCode R,Spread 0,Spacing 1,Spaces Kept,LeftMargin 18,Indent -8)

@Modify(Page,Numbered <@#@:-@1>,Referenced <@#@:-@1>)
@LibraryFile(Figures)
@LibraryFile(TitlePage)

@Modify(EquationCounter,Within Chapter)
@Modify(TheoremCounter,Within Chapter)

@Equate(Sec=Section,Subsec=SubSection,Para=Paragraph,
         SubSubSec=Paragraph,SubSubSection=Paragraph,
         AppendixSec=AppendixSection,AppendixSubSec=AppendixSubSection)
@Begin(Text,LeftMargin 1inch,TopMargin 1inch,BottomMargin 1inch,
        LineWidth 6.5in,Use BodyStyle,FaceCode R)
@Set(Page=0)
@PageHeading(Even,Left "@value(Page)")
@PageHeading(Odd,Right "@value(Page)")

@Marker(Make,GManual)
@Define(BodyStyle,Spacing 2)
@Define(TitleStyle,Spacing 1)
@Define(NoteStyle,Spacing 1)

@Generate(Notes,Outline,Index,Contents)
@Send(Notes    "@PrefaceSection(Notes)")
@Send(Contents "@PrefaceSection(Table of Contents)")
@Send(Contents "@Set(Page=1)@Style(PageNumber <@i>)")
@Send(Contents "@define(foot,invisible)")
@Send(#Index "@define(foot,invisible)")
@Send(#Index        "@UnNumbered(Index)",
      #Index        "@Begin(IndexEnv)")
@SendEnd(#Index "@End(IndexEnv)")

@Define(HDX,Hyphenation off,LeftMargin 0,RightMargin 0,Indent 0,Use B,
         Fill,Spaces compact,Above 2,Below 0,Break,Need 4,Justification Off)
@Define(Hd0,Use HdX,Above 1inch,Below 0.5inch,PageBreak UntilOdd)
@Define(Hd1,Use HdX,Below 1,PageBreak UntilOdd,Capitalized)
@Define(HD1A=HD1,Capitalized off)
@Define(Hd2,Use HdX,Above 3,Below 1)
@Define(Hd3,Use HdX)
@Define(Hd4,Use HdX)
@Define(TcX,LeftMargin 4,Indent -5,RightMargin 5,Fill,Spaces compact,
         Above 0,Spacing 1,Below 0,Break,Spread 0,Justification off)
@Define(Tc0=TcX,Use B)
@Define(Tc1=TcX,Above 1,Below 1,Use B,Need 1inch)
@Define(Tc2=TcX,LeftMargin 8)
@Define(Tc3=TcX,LeftMargin 12)
@Define(Tc4=TcX,LeftMargin 16)
@Counter(MajorPart,TitleEnv HD0,ContentsEnv tc0,Numbered [@I],
          IncrementedBy Use,Announced)
@Counter(Chapter,TitleEnv HD1,ContentsEnv tc1,Numbered [@1.],
          TitleForm
           "@begin@ParmQuote(TitleEnv)@=Chapter @parm(referenced)@~
        @*@=@Parm(Title)@end@ParmQuote(TitleEnv)",
          IncrementedBy Use,Referenced [@1],Announced)
@Counter(Appendix,TitleEnv HD1,ContentsEnv tc1,Numbered [@I.],
          IncrementedBy,Referenced [@I],Announced,Alias Chapter)
@Counter(UnNumbered,TitleEnv HD1,ContentsEnv tc1,Announced,Alias Chapter)
@Counter(Section,Within Chapter,TitleEnv HD2,ContentsEnv tc2,
          Numbered [@#@:.@1.],Referenced [@#@:.@1],IncrementedBy Use,Announced)
@Counter(AppendixSection,Within Appendix,TitleEnv HD2,ContentsEnv tc2,
          Numbered [@#@:.@1.],Referenced [@#@:.@1],IncrementedBy Use,Announced)
@Counter(SubSection,Within Section,TitleEnv HD3,ContentsEnv tc3,
          Numbered [@#@:.@1.],IncrementedBy Use,Referenced [@#@:.@1])
@Counter(AppendixSubSection,Within AppendixSection,TitleEnv Hd3,ContentsEnv Tc3,
          Numbered [@#@:.@1.],IncrementedBy USe,Referenced [@#@:.@1])
@Counter(Paragraph,Within SubSection,TitleEnv HD4,ContentsEnv tc4,
          Numbered [@#@:.@1.],Referenced [@#@:.@1],IncrementedBy Use)

@Counter(PrefaceSection,TitleEnv HD1A,Alias Chapter)
@Define(IndexEnv,Break,CRBreak,Fill,BlankLines Kept,
         Spread 0,Spacing 1,Spaces Kept,LeftMargin 18,Indent -8)

@Modify(Page,Numbered <@#@:-@1>,Referenced <@#@:-@1>)
@LibraryFile(Figures)
@LibraryFile(TitlePage)

@Modify(EquationCounter,Within Chapter)
@Modify(TheoremCounter,Within Chapter)

@Equate(Sec=Section,Subsec=SubSection,Chap=Chapter,Para=Paragraph,
         SubSubSec=Paragraph,SubSubSection=Paragraph,
         AppendixSec=AppendixSection,AppendixSubSec=AppendixSubSection)
@Begin(Text,Indent 2,Spread 1,Use BodyStyle,LineWidth 7.5inches,
        Font CharDef,FaceCode R)
@Set(Page=0)


@PageHeading(Even,Left "@value(Page)")
@PageHeading(Odd,Right "@value(Page)")

@Marker(Make,Changelog)
@Comment[
15 Jan 81  MIS  Deletion
                Removed @Style(Doublesided,BindingMargin 0.3in)
                from defs for device file, where such specifications
                are insane.
08 Feb 81  MIS  Appearance change
                Removed @Style(DoubleSided,BindingMargin 0.3 in)
                from defs for all devices with LeftMargin 0.
10 Feb 81  MIS  Deletion
                Removed XGP defs.
22 Mar 81  JMA  Appearance change
                Added X9700TempTOC and DEV to capitalize headings
                until typecase files are added to the 9700.
26 Mar 81  MIS  Creation
                Added ReGIS entry.
15 Dec 81  JMA  Deletion
                Removed X9700TempTOC and DEV now that typecases are working.
07 Jan 82  JMA  Deletion
                Removed the second (and misspelled) hyphenation off
                declaration from  HDX for REGIS.
07 Jan 82  JMA  Creation
                Added entry for GSI.
23 Apr 82  JMA  Creation
                Added definition for Cg8600, and put the majority
                of the declarations into PhotocomposerManual.Lib.
16 May 82  JMA  Creation
                Added X5700 and X9700 to the marker for the dover
                (PRESS) entry.
16 May 82  JMA  Deletion
                Deleted the X9700.  The only difference had been
                the capitalized attribute to HD0 and HD1 which was required
                in the days before typecase files.
16 May 82  JMA  Creation
                Added Cg8600 to the marker for the Omnitech entry.
16 May 82  JMA  Edit
                Replaced FontFamily commands in the dover and omnitech
                entries with LibraryFile(SansSerif) or LibraryFile(Serif).
03 Jun 82  JMA  Appearance change
                Separated Omni and 8600.  Changed the leftmargin from
                1 inh to 0.5 inch so that the total (including binding
                margin) would be less than 7.5 inch.
 9 Jun 82  JMA  Creation
                Added VIP to the Cg8600 entry.
14 Oct 82  MEF  Appearance change
                Added "Hyphenation Standby" to level-2 begin.
03 Dec 82  MIS  Appearance change
                Altered @Send in 9700 entry so that TOC is guaranteed
                not to fall on the reverse of the last page of the index.
20 Jan 83  MEF  Creation
                Added LGP1 to ImPress entry.
24 Jan 83  MIS  Creation
                Added Sanders entry.
18 Feb 83  EMR  Appearance change
                Added PageBreak UntilOdd to HD0 for MajorPart counter.
26 May 83  EMN  Creation
                Added X2700 to Impress entry.
28 Jul 83  EMN  Addition
                Added SubSubSection=Paragraph AppendixSec=AppendixSection
                to synonyms in Equate commands.
                Change request A-100,A-209
27 Sep 83  MMC  Edit
                Changed the format of the changelog.
27 Sep 83  MMC  Creation
                Added NonScaleableLaser to x2700 entry.
18 Nov 83  EMN  Change
                Changed TypeWheel style keyword to FontFamily.
18 Nov 83  EMN  Appearance Change
                Changed Leftmargin specifications for Tc1-Tc4 from 5,5,10,15
                to 4,8,12,16 for devices: Robottypewriter,Sanders,File,Crt,
                Pagedfile, and the Default.
18 Nov 83  MMC  Edit
                Changed the TitleForm and ContentsForm of several entries
                from, for instance, @begin(tc1) or @tc1"  " to
                @Begin@ParmQuote(ContentsEnv).
21 Nov 83  EMN  Appearance Change
                Added NOTES to the @Generate in each Marker entry and
                @Send(Notes    "@PrefaceSection(Notes)") to create a heading
                on the Notes page.
 6 Dec 83  EMN  Change
                Changed @Enable to @Generate
13 Dec 83  EMN  Addition
                Added @Send(Contents "@define(foot,invisible)")
                and @Send(#Index "@define(foot,invisible)").
13 Dec 83  EMN  Change
                Changed device name Sanders to Santec.
13 Dec 83  EMN  Deletion
                Deleted x5700 entry.
16 Dec 83  EMN  Deletion
                Deleted LGP1 entry. LGP1 uses Impress entry as Generic Device.
                Deleted Pagedfile and CRT entries; they use the File entry
                as Generic Device.
19 Sep 84  EMN  Addition
                Added Impress300,Impress480 to the Impress Marker entry.
 7 Jan 85  MEF  Addition
                Added ScaleableLaser to Omnitech entry.
 6 Aug 85  EMN  Addition
                Added "Quic" to NonScaleable entry for device Lg1200.
11 Dec 85  EMN  Addition
                Added `Tabexport False,Afterentry "@tabclear()"' to definition
                of HD1A.
11 Dec 85  EMN  Addition
                Added @Modify(Page,Numbered <@#@:-@1>,Referenced <@#@:-@1>)
                so that users who want pages numbered within chapter
                can easily do so with @Modify(Page, Within Chapter).
 8 Apr 86  EMN  Addition
                Added Imagen 12/300 entry.
29 Apr 87  EMN  Edit
                Changed the order of the Send commands to the Contents
                for all devices so that the @Style(PageNumber) command appears
                after the @Set(Page=1).  (Deleted @Process[Pageheadings])
]


