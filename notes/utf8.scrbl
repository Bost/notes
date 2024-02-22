#lang notes

@block{@block-name{Utf8}
  `utf8lookup' - look up Unicode characters from the command line.
  Doesn't work on Guix. E.g. `utf8lookup 1254`

  slovak sk diacritics á ä č ď é í ĺ ľ ň ó ô ŕ š ť ú ý ž  Á Ä Č Ď É Í Ĺ Ľ Ň Ó Ô Ŕ Š Ť Ú Ý Ž
  french fr diacritics à â ä æ ç è é ê ë î ï ô œ ù û ü    À Â Ä Æ Ç È É Ê Ë Î Ï Ô Œ Ù Û Ü Œ
  german de diacritics ü ö ä  Ä Ü Ö ß

  Exists / forall / for-all / for all / every ∀ ∃
  Category ℂ
  Composition / "after" ∘
  Numbers ➊ ➋ ➌ ➍ ➎ ➏ ➐ ➑ ➒
  Table ⊥ ⊢ ⊣ ⊤
  Thin arrows ← ↑ → ↓ ↔ ↕ ↖ ↗ ↘ ↙
  Open headed arrows ⇽ ⇾ ⇿
  Double arrows (implication) ⇐ ⇑ ⇒ ⇓ ⇔ ⇕
  Lines ⎛ ⎜ ⎝ ⎞ ⎟ ⎠ ⎡ ⎢ ⎣ ⎤ ⎥ ⎦ ⎧ ⎨ ⎩ ⎪ ⎫ ⎬ ⎭ ⎮ ⎯
  Brackets ⟦ ⟧ ⟨ ⟩ ⟪ ⟫ ⟬ ⟭ ⟮ ⟯⦃ ⦄ ⦅ ⦆ ⦇ ⦈ ⦉ ⦊ ⦋ ⦌ ⦍ ⦎ ⦏⦐⦑⦒⦓⦔⦕ ⦖ ⦗ ⦘

  GREEK SMALL LETTER ALPHA	α	&‌#945;	&‌#x03B1	&‌alpha;
  GREEK SMALL LETTER BETA	β	&‌#946;	&‌#x03B2	&‌beta;
  GREEK SMALL LETTER GAMMA	γ	&‌#947;	&‌#x03B3	&‌gamma;
  GREEK SMALL LETTER DELTA	δ	&‌#948;	&‌#x03B4	&‌delta;
  GREEK SMALL LETTER EPSILON	ε	&‌#949;	&‌#x03B5	&‌epsilon;
  curly epsilon (ε): \xce\xb5
  straight epsilon (ϵ): \xcf\xb5
  GREEK SMALL LETTER ZETA	ζ	&‌#950;	&‌#x03B6	&‌zeta;
  GREEK SMALL LETTER ETA	η	&‌#951;	&‌#x03B7	&‌eta;
  GREEK SMALL LETTER THETA	θ	&‌#952;	&‌#x03B8	&‌theta;
  GREEK SMALL LETTER IOTA	ι	&‌#953;	&‌#x03B9	&‌iota;
  GREEK SMALL LETTER KAPPA	κ	&‌#954;	&‌#x03BA	&‌kappa;
  GREEK SMALL LETTER LAM(B)DA	λ	&‌#955;	&‌#x03BB	&‌lambda;
  GREEK SMALL LETTER MU	μ	&‌#956;	&‌#x03BC	&‌mu;
  GREEK SMALL LETTER NU	ν	&‌#957;	&‌#x03BD	&‌nu;
  GREEK SMALL LETTER XI	ξ	&‌#958;	&‌#x03BE	&‌xi;
  GREEK SMALL LETTER OMICRON	ο	&‌#959;	&‌#x03BF	&‌omicron;
  GREEK SMALL LETTER PI	π	&‌#960;	&‌#x03C0	&‌pi;
  GREEK SMALL LETTER RHO	ρ	&‌#961;	&‌#x03C1	&‌rho;
  GREEK SMALL LETTER FINAL SIGMA	ς	&‌#962;	&‌#x03C2
  GREEK SMALL LETTER SIGMA	σ	&‌#963;	&‌#x03C3	&‌sigma;
  GREEK SMALL LETTER TAU	τ	&‌#964;	&‌#x03C4	&‌tau;
  GREEK SMALL LETTER UPSILON	υ	&‌#965;	&‌#x03C5	&‌upsilon;
  GREEK SMALL LETTER PHI	φ	&‌#966;	&‌#x03C6	&‌phi;
  GREEK SMALL LETTER CHI	χ	&‌#967;	&‌#x03C7	&‌chi;
  GREEK SMALL LETTER PSI	ψ	&‌#968;	&‌#x03C8	&‌psi;
  GREEK SMALL LETTER OMEGA	ω	&‌#969;	&‌#x03C9	&‌omega;
  GREEK CAPITAL LETTER ALPHA	Α	&‌#913;	&‌#x0391	&‌Alpha;
  GREEK CAPITAL LETTER BETA	Β	&‌#914;	&‌#x0392	&‌Beta;
  GREEK CAPITAL LETTER GAMMA	Γ	&‌#915;	&‌#x0393	&‌Gamma;
  GREEK CAPITAL LETTER DELTA	Δ	&‌#916;	&‌#x0394	&‌Delta;
  GREEK CAPITAL LETTER EPSILON	Ε	&‌#917;	&‌#x0395	&‌Epsilon;
  GREEK CAPITAL LETTER ZETA	Ζ	&‌#918;	&‌#x0396	&‌Zeta;
  GREEK CAPITAL LETTER ETA	Η	&‌#919;	&‌#x0397	&‌Eta;
  GREEK CAPITAL LETTER THETA	Θ	&‌#920;	&‌#x0398	&‌Theta;
  GREEK CAPITAL LETTER IOTA	Ι	&‌#921;	&‌#x0399	&‌Iota;
  GREEK CAPITAL LETTER KAPPA	Κ	&‌#922;	&‌#x039A	&‌Kappa;
  GREEK CAPITAL LETTER LAM(B)DA	Λ	&‌#923;	&‌#x039B	&‌Lambda;
  GREEK CAPITAL LETTER MU	Μ	&‌#924;	&‌#x039C	&‌Mu;
  GREEK CAPITAL LETTER NU	Ν	&‌#925;	&‌#x039D	&‌Nu;
  GREEK CAPITAL LETTER XI	Ξ	&‌#926;	&‌#x039E	&‌Xi;
  GREEK CAPITAL LETTER OMICRON	Ο	&‌#927;	&‌#x039F	&‌Omicron;
  GREEK CAPITAL LETTER PI	Π	&‌#928;	&‌#x03A0	&‌Pi;
  GREEK CAPITAL LETTER RHO	Ρ	&‌#929;	&‌#x03A1	&‌Rho;
  GREEK CAPITAL LETTER SIGMA	Σ	&‌#931;	&‌#x03A3	&‌Sigma;
  GREEK CAPITAL LETTER TAU	Τ	&‌#932;	&‌#x03A4	&‌Tau;
  GREEK CAPITAL LETTER UPSILON	Υ	&‌#933;	&‌#x03A5	&‌Upsilon;
  GREEK CAPITAL LETTER PHI	Φ	&‌#934;	&‌#x03A6	&‌Phi;
  GREEK CAPITAL LETTER CHI	Χ	&‌#935;	&‌#x03A7	&‌Chi;
  GREEK CAPITAL LETTER PSI	Ψ	&‌#936;	&‌#x03A8	&‌Psi;
  GREEK CAPITAL LETTER OMEGA	Ω	&‌#937;	&‌#x03A9	&‌Omega;

  superscript ⁰ ¹ ² ³ ⁴ ⁵ ⁶ ⁷ ⁸ ⁹ ⁺ ⁻ ⁼ ⁽ ⁾ ⁿ ⁱ
  subscript ₀ ₁ ₂ ₃ ₄ ₅ ₆ ₇ ₈ ₉ ₊ ₋ ₌ ₍ ₎ ₐ ₑ ₕ ᵢ ⱼ ₖ ₗ ₘ ₙ ₒ ₚ ᵣ ₛ ₜ ᵤ ᵥ ₓ ₔ
  Roots √ ∛ ∜
  Numbers: Natural ℕ, Integer ℤ, Rational ℚ, Real ℝ, Complex ℂ, Quaternion ℍ
  imaginary number ⅈ, ⅉ
  Euler's number, base of natural log ℯ, ⅇ
  EULER CONSTANT ℇ
  Infinity ∞ ⧜ ⧝ ⧞
  multiplication, division, product, coproduct × ✕ ✖ ÷
  − ∕ ∗ ∘ ∙ ⋅ ⋆
  empty set ∅
  Aleph ℵ
  element of ∈ ∋ ∉ ∌ ⋶ ⋽ ⋲ ⋺ ⋳ ⋻
  misc ∊ ∍ ⋷ ⋾ ⋴ ⋼ ⋵ ⋸ ⋹ ⫙ ⟒
  Union ∪ ⩁ ⩂ ⩅ ⩌ ⩏ ⩐
  Intersection ∩ ⩀ ⩃ ⩄ ⩍ ⩎
  Binary operator on sets ∖ ⩆ ⩇ ⩈ ⩉ ⩊ ⩋ ⪽ ⪾ ⪿ ⫀ ⫁ ⫂ ⋒ ⋓
  N-nary operator on sets ⋂ ⋃ ⊌ ⊍ ⊎ ⨃ ⨄ ⨅ ⨆
  Joins ⨝ ⟕ ⟖ ⟗
  Precede and succeed ≺ ≻ ≼ ≽ ≾ ≿ ⊀ ⊁ ⋞ ⋟ ⋠ ⋡ ⋨ ⋩ ⪯ ⪰ ⪱ ⪲ ⪳ ⪴ ⪵ ⪶ ⪷ ⪸ ⪹ ⪺ ⪻ ⪼
  less and greater < > ≮ ≯ ≤ ≥ ≰ ≱ ⪇ ⪈ ≦ ≧ ≨ ≩
  less and greater 2 ⋜ ⋝ ⪙ ⪚ ≶ ≷ ≸ ≹ ⋚ ⋛ ⪋ ⪌ ⪑ ⪒ ⪓ ⪔
  with approx ⪅ ⪆ ⪉ ⪊
  less and greater with equivalence ≲ ≳ ⋦ ⋧ ≴ ≵
  less and greater with similarity ⪝ ⪞ ⪟ ⪠ ⪍ ⪎ ⪏ ⪐
  less and greater slanted ⩽ ⩾ ⫹ ⫺ ⪕ ⪖ ⪛ ⪜
  less and greater misc ⪣ ⪤ ⪥ ⪦ ⪧ ⪨ ⪩ ⪪ ⪫ ⪬ ⪭ ⪡ ⪢ ⫷ ⫸ ⩹ ⩺ ⩻ ⩼ ≪ ≫ ⋘ ⋙ ≬
  Order relation with dot ⋖ ⋗ ⩿ ⪀ ⪗ ⪘ ⪁ ⪂ ⪃ ⪄
  Equality, Identity, Equivalence, Approx, Congruence equality ≍ ≭ ≣ ⩸ ≡ ≢ ⩧ ≝ ≞ ≟ ≠ ∹ ≎ ≏ ⪮ ≐ ≑ ≒ ≓ ≔ ≕ ≖ ≗ ≘ ≙ ≚ ≛ ≜ ⩬ ⩭ ⩮ ⩱ ⩲ ⩦ ⩴ ⩵ ⩶ ⩷
  Approx / almost / asymptotic equality / equivalence ≁ ≂ ≃ ≄ ⋍ ≅ ≆ ≇ ≈ ≉ ≊ ≋ ≌ ⩯ ⩰
  isomorphism ⋍
  Misc equality ∻
  Misc relations ⊏ ⊐ ⊑ ⊒ ⊓ ⊔ ⋢ ⋣ ⋤ ⋥ ⫴ ⫵
  Normal subgroups ⊲ ⊳ ⊴ ⊵ ⋪ ⋫ ⋬ ⋭
  Logic ¬ ⫬ ⫭ ⊨ ⊭ ∀ ∁ ∃ ∄ ∴ ∵ ⊦ ⊬ ⊧ ⊩ ⊮ ⊫ ⊯ ⊪ ⊰ ⊱
  Logic binary (conjunction, disjunction) ∧ ∨ ⊻ ⊼ ⊽ ⋎ ⋏ ⟑ ⟇ ⩑ ⩒ ⩓ ⩔ ⩕ ⩖ ⩗ ⩘ ⩙ ⩚ ⩛ ⩜ ⩝ ⩞ ⩟ ⩠ ⩢ ⩣ ⨇ ⨈
  Logic n-nary (conjunction, disjunction) ⋀ ⋁
  Geometry ∣ ∤ ⫮ ⌅ ⌆ ℓ ⫛
  Ratio, proportion ∝ ∶ ∷ ∺
  Parallel, perpendicular ∥ ∦ ⫲ ⫳ ⋕ ⟂ ⫡
  Right angle ⦜ ∟ ⊾ ⦝ ⊿
  Angles ∠ ∡ ⦛ ⦞ ⦟ ⦢ ⦣ ⦤ ⦥ ⦦ ⦧ ⦨ ⦩ ⦪ ⦫ ⦬ ⦭ ⦮ ⦯ ⦓ ⦔ ⦕ ⦖ ⟀
  Spherical angle ∢ ⦠ ⦡
  Bracket operators ⌈ ⌉ ⌊ ⌋ ⫍ ⫎
  integrals ∫ ∬ ∭ ∮ ∯ ∰ ∱ ∲ ∳ ⨋ ⨌ ⨍ ⨎ ⨏ ⨐ ⨑ ⨒ ⨓ ⨔ ⨕ ⨖ ⨗ ⨘ ⨙ ⨚ ⨛ ⨜
  Derivative ∂ ′ ″ ‴ ∆
  Cartesian Product / vector ⨯ ∇ ⊹
  Tilde Operators ∼ ∽ ⩪ ⩫ ⩳
  Misc Operators ⋄ ⫶ ⫼ ⫾
  Misc products ≀ ⨿ ⨼ ⨽ ⧢ ⋉ ⋊ ⋋ ⋌
  n-nary sum ∑ ⨊ ⨁
  n-nary product ⨀ ⨂ ∏ ∐ ⨉
  Mathematica ⧴
  Plus ⨢ ⨣ ⨤ ⨥ ⨦ ⨧ ⨨ ⨭ ⨮
  ∔ ⧺ ⧻
  minus sign ∸ ⨩ ⨪ ⨫ ⨬
  multiplication, product ⨰ ⨱ ⨲ ⨳
  division ⋇ ⟌ ⟠
  Misc indicators ∎ ± ∓ ⋮ ⋯ ⋰ ⋱
  Misc symbols ∿
  Tacks ⊣ ⊢ ⊥ ⊤ ⟘ ⟙ ⟛ ⟝ ⟞ ⟟ ⫧ ⫨ ⫩ ⫪ ⫫ ⫞ ⫟ ⫠
  Turnstiles ⫢ ⫣ ⫤ ⫥ ⟚
  Z notation ⦁ ⦂ ⨾ ⨟ ⨠ ⨡ ⩤ ⩥ ⦇ ⦈ ⦉ ⦊
  Solidus, slash ⧵ ⧶ ⧷ ⧸ ⫽ ⫻ ⧹
  maps, mapping, transform ⊶ ⊷ ⊸ ⟜ ⧟
  empty set ⦰ ⦱ ⦲ ⦳ ⦴
  Unsorted ⦵ ⦶ ⦷ ⦸ ⦹ ⦺ ⦻ ⨴ ⨵ ⨶ ⨷ ⨸ ⦼
  Unsorted ⦽ ⧀ ⧁ ⧂ ⧃
  Unsorted ⧡ ⧣ ⧤ ⧥ ⧦ ⧧
  Unsorted ⧾ ⨞ ⧊ ⧋ ⧌ ⧍ ⨹ ⨺ ⨻
  Unsorted ⧎ ⧏ ⧐ ⩡ ⩨ ⩩ ⫝̸ ⫝ ⫦ ⫯ ⫰ ⫱
  Unsorted ∾ ⊺ ⋔ ⫚ ⟊ ⟔ ⟓ ⟡ ⟢ ⟣ ⟤ ⟥
  Unicode: Arrow Symbols ← → ↑ ↓
  Unicode: Brackets, Quotes «»「」【】《》
  Unicode Box Lines, Shapes ┌ ┬ ┐
  Unicode Geometric Shapes ◩ ◐ ⋈ ⬢
  APL Programing Language Symbols
  There are more math symbols but are outside of BMP. In particular, there are several complete set of styled English alphabet, such as double-struck chars (ℂ ℝ ⅈ ⅉ) gothic-styled letters (ℭ ℑ ℌ ℜ ℨ), scripted letter forms (ℓ ℱ ℒ ℛ).
  For a complete list, see:
  Math Font ℤ ℚ ℝ ℂ ℜ ℑ ℵ
  Unicode: Greek Alphabet α β γ
  ~ TILDE
  ∼ TILDE OPERATOR
  ∽ REVERSED TILDE
  〜 WAVE DASH
  ∿ SINE WAVE
  ≈ ALMOST EQUAL TO
  Another example:
  ⩳ EQUALS SIGN ABOVE TILDE OPERATOR
  ≌ ALL EQUAL TO
  ⩯ ALMOST EQUAL TO WITH CIRCUMFLEX ACCENT
  ⩰ APPROXIMATELY EQUAL OR EQUAL TO

  ■	9632	25A0	  BLACK SQUARE
  □	9633	25A1	  WHITE SQUARE
  ▢	9634	25A2	  WHITE SQUARE WITH ROUNDED CORNERS
  ▣	9635	25A3	  WHITE SQUARE CONTAINING BLACK SMALL SQUARE
  ▤	9636	25A4	  SQUARE WITH HORIZONTAL FILL
  ▥	9637	25A5	  SQUARE WITH VERTICAL FILL
  ▦	9638	25A6	  SQUARE WITH ORTHOGONAL CROSSHATCH FILL
  ▧	9639	25A7	  SQUARE WITH UPPER LEFT TO LOWER RIGHT FILL
  ▨	9640	25A8	  SQUARE WITH UPPER RIGHT TO LOWER LEFT FILL
  ▩	9641	25A9	  SQUARE WITH DIAGONAL CROSSHATCH FILL
  ▪	9642	25AA	  BLACK SMALL SQUARE
  ▫	9643	25AB	  WHITE SMALL SQUARE
  ▬	9644	25AC	  BLACK RECTANGLE
  ▭	9645	25AD	  WHITE RECTANGLE
  ▮	9646	25AE	  BLACK VERTICAL RECTANGLE
  ▯	9647	25AF	  WHITE VERTICAL RECTANGLE
  ▰	9648	25B0	  BLACK PARALLELOGRAM
  ▱	9649	25B1	  WHITE PARALLELOGRAM
  ▲	9650	25B2	  BLACK UP-POINTING TRIANGLE
  △	9651	25B3	  WHITE UP-POINTING TRIANGLE
  ▴	9652	25B4	  BLACK UP-POINTING SMALL TRIANGLE
  ▵	9653	25B5	  WHITE UP-POINTING SMALL TRIANGLE
  ▶	9654	25B6	  BLACK RIGHT-POINTING TRIANGLE
  ▷	9655	25B7	  WHITE RIGHT-POINTING TRIANGLE
  ▸	9656	25B8	  BLACK RIGHT-POINTING SMALL TRIANGLE
  ▹	9657	25B9	  WHITE RIGHT-POINTING SMALL TRIANGLE
  ►	9658	25BA	  BLACK RIGHT-POINTING POINTER
  ▻	9659	25BB	  WHITE RIGHT-POINTING POINTER
  ▼	9660	25BC	  BLACK DOWN-POINTING TRIANGLE
  ▽	9661	25BD	  WHITE DOWN-POINTING TRIANGLE
  ▾	9662	25BE	  BLACK DOWN-POINTING SMALL TRIANGLE
  ▿	9663	25BF	  WHITE DOWN-POINTING SMALL TRIANGLE
  ◀	9664	25C0	  BLACK LEFT-POINTING TRIANGLE
  ◁	9665	25C1	  WHITE LEFT-POINTING TRIANGLE
  ◂	9666	25C2	  BLACK LEFT-POINTING SMALL TRIANGLE
  ◃	9667	25C3	  WHITE LEFT-POINTING SMALL TRIANGLE
  ◄	9668	25C4	  BLACK LEFT-POINTING POINTER
  ◅	9669	25C5	  WHITE LEFT-POINTING POINTER
  ◆	9670	25C6	  BLACK DIAMOND
  ◇	9671	25C7	  WHITE DIAMOND
  ◈	9672	25C8	  WHITE DIAMOND CONTAINING BLACK SMALL DIAMOND
  ◉	9673	25C9	  FISHEYE
  ◊	9674	25CA	&loz;	LOZENGE
  ○	9675	25CB	  WHITE CIRCLE
  ◌	9676	25CC	  DOTTED CIRCLE
  ◍	9677	25CD	  CIRCLE WITH VERTICAL FILL
  ◎	9678	25CE	  BULLSEYE
  ●	9679	25CF	  BLACK CIRCLE
  ◐	9680	25D0	  CIRCLE WITH LEFT HALF BLACK
  ◑	9681	25D1	  CIRCLE WITH RIGHT HALF BLACK
  ◒	9682	25D2	  CIRCLE WITH LOWER HALF BLACK
  ◓	9683	25D3	  CIRCLE WITH UPPER HALF BLACK
  ◔	9684	25D4	  CIRCLE WITH UPPER RIGHT QUADRANT BLACK
  ◕	9685	25D5	  CIRCLE WITH ALL BUT UPPER LEFT QUADRANT BLACK
  ◖	9686	25D6	  LEFT HALF BLACK CIRCLE
  ◗	9687	25D7	  RIGHT HALF BLACK CIRCLE
  ◘	9688	25D8	  INVERSE BULLET
  ◙	9689	25D9	  INVERSE WHITE CIRCLE
  ◚	9690	25DA	  UPPER HALF INVERSE WHITE CIRCLE
  ◛	9691	25DB	  LOWER HALF INVERSE WHITE CIRCLE
  ◜	9692	25DC	  UPPER LEFT QUADRANT CIRCULAR ARC
  ◝	9693	25DD	  UPPER RIGHT QUADRANT CIRCULAR ARC
  ◞	9694	25DE	  LOWER RIGHT QUADRANT CIRCULAR ARC
  ◟	9695	25DF	  LOWER LEFT QUADRANT CIRCULAR ARC
  ◠	9696	25E0	  UPPER HALF CIRCLE
  ◡	9697	25E1	  LOWER HALF CIRCLE
  ◢	9698	25E2	  BLACK LOWER RIGHT TRIANGLE
  ◣	9699	25E3	  BLACK LOWER LEFT TRIANGLE
  ◤	9700	25E4	  BLACK UPPER LEFT TRIANGLE
  ◥	9701	25E5	  BLACK UPPER RIGHT TRIANGLE
  ◦	9702	25E6	  WHITE BULLET
  ◧	9703	25E7	  SQUARE WITH LEFT HALF BLACK
  ◨	9704	25E8	  SQUARE WITH RIGHT HALF BLACK
  ◩	9705	25E9	  SQUARE WITH UPPER LEFT DIAGONAL HALF BLACK
  ◪	9706	25EA	  SQUARE WITH LOWER RIGHT DIAGONAL HALF BLACK
  ◫	9707	25EB	  WHITE SQUARE WITH VERTICAL BISECTING LINE
  ◬	9708	25EC	  WHITE UP-POINTING TRIANGLE WITH DOT
  ◭	9709	25ED	  UP-POINTING TRIANGLE WITH LEFT HALF BLACK
  ◮	9710	25EE	  UP-POINTING TRIANGLE WITH RIGHT HALF BLACK
  ◯	9711	25EF	  LARGE CIRCLE
  ◰	9712	25F0	  WHITE SQUARE WITH UPPER LEFT QUADRANT
  ◱	9713	25F1	  WHITE SQUARE WITH LOWER LEFT QUADRANT
  ◲	9714	25F2	  WHITE SQUARE WITH LOWER RIGHT QUADRANT
  ◳	9715	25F3	  WHITE SQUARE WITH UPPER RIGHT QUADRANT
  ◴	9716	25F4	  WHITE CIRCLE WITH UPPER LEFT QUADRANT
  ◵	9717	25F5	  WHITE CIRCLE WITH LOWER LEFT QUADRANT
  ◶	9718	25F6	  WHITE CIRCLE WITH LOWER RIGHT QUADRANT
  ◷	9719	25F7	  WHITE CIRCLE WITH UPPER RIGHT QUADRANT
  ◸	9720	25F8	  UPPER LEFT TRIANGLE
  ◹	9721	25F9	  UPPER RIGHT TRIANGLE
  ◺	9722	25FA	  LOWER LEFT TRIANGLE
  ◻	9723	25FB	  WHITE MEDIUM SQUARE
  ◼	9724	25FC	  BLACK MEDIUM SQUARE
  ◽	9725	25FD	  WHITE MEDIUM SMALL SQUARE
  ◾	9726	25FE	  BLACK MEDIUM SMALL SQUARE
  ◿	9727	25FF	  LOWER RIGHT TRIANGLE
}
