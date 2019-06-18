<map version="freeplane 1.7.0">
<!--To view this file, download free mind mapping software Freeplane from http://freeplane.sourceforge.net -->
<node TEXT="maxima-to-ir" FOLDED="false" ID="ID_358261271" CREATED="1558686007510" MODIFIED="1559071723996" STYLE="oval">
<font SIZE="18"/>
<hook NAME="MapStyle" zoom="1.5" layout="OUTLINE">
    <properties edgeColorConfiguration="#808080ff,#ff0000ff,#0000ffff,#00ff00ff,#ff00ffff,#00ffffff,#7c0000ff,#00007cff,#007c00ff,#7c007cff,#007c7cff,#7c7c00ff" show_icon_for_attributes="true" show_note_icons="true" fit_to_viewport="false"/>

<map_styles>
<stylenode LOCALIZED_TEXT="styles.root_node" STYLE="oval" UNIFORM_SHAPE="true" VGAP_QUANTITY="24.0 pt">
<font SIZE="24"/>
<stylenode LOCALIZED_TEXT="styles.predefined" POSITION="right" STYLE="bubble">
<stylenode LOCALIZED_TEXT="default" ICON_SIZE="12.0 pt" COLOR="#000000" STYLE="fork">
<font NAME="SansSerif" SIZE="10" BOLD="false" ITALIC="false"/>
</stylenode>
<stylenode LOCALIZED_TEXT="defaultstyle.details"/>
<stylenode LOCALIZED_TEXT="defaultstyle.attributes">
<font SIZE="9"/>
</stylenode>
<stylenode LOCALIZED_TEXT="defaultstyle.note" COLOR="#000000" BACKGROUND_COLOR="#ffffff" TEXT_ALIGN="LEFT"/>
<stylenode LOCALIZED_TEXT="defaultstyle.floating">
<edge STYLE="hide_edge"/>
<cloud COLOR="#f0f0f0" SHAPE="ROUND_RECT"/>
</stylenode>
</stylenode>
<stylenode LOCALIZED_TEXT="styles.user-defined" POSITION="right" STYLE="bubble">
<stylenode LOCALIZED_TEXT="styles.topic" COLOR="#18898b" STYLE="fork">
<font NAME="Liberation Sans" SIZE="10" BOLD="true"/>
</stylenode>
<stylenode LOCALIZED_TEXT="styles.subtopic" COLOR="#cc3300" STYLE="fork">
<font NAME="Liberation Sans" SIZE="10" BOLD="true"/>
</stylenode>
<stylenode LOCALIZED_TEXT="styles.subsubtopic" COLOR="#669900">
<font NAME="Liberation Sans" SIZE="10" BOLD="true"/>
</stylenode>
<stylenode LOCALIZED_TEXT="styles.important">
<icon BUILTIN="yes"/>
</stylenode>
</stylenode>
<stylenode LOCALIZED_TEXT="styles.AutomaticLayout" POSITION="right" STYLE="bubble">
<stylenode LOCALIZED_TEXT="AutomaticLayout.level.root" COLOR="#000000" STYLE="oval" SHAPE_HORIZONTAL_MARGIN="10.0 pt" SHAPE_VERTICAL_MARGIN="10.0 pt">
<font SIZE="18"/>
</stylenode>
<stylenode LOCALIZED_TEXT="AutomaticLayout.level,1" COLOR="#0033ff">
<font SIZE="16"/>
</stylenode>
<stylenode LOCALIZED_TEXT="AutomaticLayout.level,2" COLOR="#00b439">
<font SIZE="14"/>
</stylenode>
<stylenode LOCALIZED_TEXT="AutomaticLayout.level,3" COLOR="#990000">
<font SIZE="12"/>
</stylenode>
<stylenode LOCALIZED_TEXT="AutomaticLayout.level,4" COLOR="#111111">
<font SIZE="10"/>
</stylenode>
<stylenode LOCALIZED_TEXT="AutomaticLayout.level,5"/>
<stylenode LOCALIZED_TEXT="AutomaticLayout.level,6"/>
<stylenode LOCALIZED_TEXT="AutomaticLayout.level,7"/>
<stylenode LOCALIZED_TEXT="AutomaticLayout.level,8"/>
<stylenode LOCALIZED_TEXT="AutomaticLayout.level,9"/>
<stylenode LOCALIZED_TEXT="AutomaticLayout.level,10"/>
<stylenode LOCALIZED_TEXT="AutomaticLayout.level,11"/>
</stylenode>
</stylenode>
</map_styles>
</hook>
<hook NAME="AutomaticEdgeColor" COUNTER="3" RULE="ON_BRANCH_CREATION"/>
<cloud COLOR="#f0f0f0" SHAPE="ROUND_RECT"/>
<hook NAME="accessories/plugins/AutomaticLayout.properties" VALUE="ALL"/>
<node TEXT="atom" POSITION="right" ID="ID_1798541669" CREATED="1558686103789" MODIFIED="1559025499629" HGAP_QUANTITY="4.250000290572633 pt" VSHIFT_QUANTITY="-8.249999754130847 pt">
<edge COLOR="#ff0000"/>
<node TEXT="$%i" ID="ID_425784385" CREATED="1558686112963" MODIFIED="1558686121485">
<node TEXT="(num 0 1)" ID="ID_981644649" CREATED="1558686121493" MODIFIED="1558686132850"/>
</node>
<node TEXT="$%pi" ID="ID_1170399689" CREATED="1558763346503" MODIFIED="1558763358727">
<node TEXT="(num (symbol &quot;math.pi&quot;) 0)" ID="ID_360510773" CREATED="1558763358738" MODIFIED="1559808758027"/>
</node>
<node TEXT="$%e" ID="ID_1397422185" CREATED="1558763399655" MODIFIED="1558763405351">
<node TEXT="(num (symbol &quot;math.e&quot;) 0)" ID="ID_1992735641" CREATED="1558763410422" MODIFIED="1559808769711"/>
</node>
<node TEXT="(stringp form)" ID="ID_1225043634" CREATED="1559025411013" MODIFIED="1559025434843" HGAP_QUANTITY="16.999999910593036 pt" VSHIFT_QUANTITY="6.74999979883433 pt">
<node TEXT="(string form)" ID="ID_706078835" CREATED="1559025422633" MODIFIED="1559025429708"/>
</node>
<node TEXT="DEFAULT" ID="ID_1038463786" CREATED="1559821647966" MODIFIED="1559821654812">
<node TEXT="(num form 0)" ID="ID_140096892" CREATED="1559821654818" MODIFIED="1559821661209"/>
</node>
</node>
<node TEXT="(consp form)" POSITION="left" ID="ID_1348542721" CREATED="1558783725856" MODIFIED="1559025494346" HGAP_QUANTITY="5.000000268220894 pt" VSHIFT_QUANTITY="-7.499999776482588 pt">
<edge COLOR="#0000ff"/>
<node TEXT="(consp (car form))" ID="ID_238191698" CREATED="1558783980523" MODIFIED="1559025474297" HGAP_QUANTITY="11.750000067055224 pt" VSHIFT_QUANTITY="-9.749999709427364 pt">
<node TEXT="((atom ~) ~)" ID="ID_1483328184" CREATED="1558783994895" MODIFIED="1559205519901" HGAP_QUANTITY="11.750000067055224 pt" VSHIFT_QUANTITY="-10.499999687075626 pt">
<node TEXT="(atom (caar form))" FOLDED="true" ID="ID_1347493343" CREATED="1558784008079" MODIFIED="1559205503181" HGAP_QUANTITY="826.9999757707126 pt" VSHIFT_QUANTITY="-14.999999552965177 pt">
<node TEXT="((mtimes ~) exp1 exp2 ~)" ID="ID_1425557450" CREATED="1558784069181" MODIFIED="1559205503180" HGAP_QUANTITY="-1152.2499652430424 pt" VSHIFT_QUANTITY="5735.249829076236 pt">
<node TEXT="(op * simp-exp1 simp-exp2 ~)" ID="ID_1877074756" CREATED="1558784151010" MODIFIED="1559124513202" HGAP_QUANTITY="-331.7499896958473 pt" VSHIFT_QUANTITY="2.9999999105930377 pt"/>
</node>
<node TEXT="((mplus ~) exp1 exp2 ~)" ID="ID_577061991" CREATED="1558784193235" MODIFIED="1559205496210" HGAP_QUANTITY="-167.49999459087863 pt" VSHIFT_QUANTITY="870.7499740496285 pt">
<node TEXT="(op + simp-exp1 simp-exp2 ~)" ID="ID_1681649521" CREATED="1558784240938" MODIFIED="1559205487506" HGAP_QUANTITY="-380.49998824298416 pt" VSHIFT_QUANTITY="46.49999861419206 pt"/>
</node>
<node TEXT="((mexpt ~) exp1 exp2)" ID="ID_1366568193" CREATED="1558784216863" MODIFIED="1559124534907" HGAP_QUANTITY="70.24999832361942 pt" VSHIFT_QUANTITY="-131.99999606609356 pt">
<node TEXT="(funcall (symbol &quot;pow&quot;) simp-exp1 simp-exp2)" ID="ID_1195580292" CREATED="1558784358428" MODIFIED="1559799986211" HGAP_QUANTITY="34.24999939650299 pt" VSHIFT_QUANTITY="-6.749999798834329 pt"/>
</node>
<node TEXT="((mfactorial ~) exp1)" ID="ID_84710372" CREATED="1558784291690" MODIFIED="1559124538506" HGAP_QUANTITY="341.74999023228906 pt" VSHIFT_QUANTITY="-362.9999891817573 pt">
<node TEXT="(funcall math.factorial simp-exp1)" ID="ID_1412481236" CREATED="1558784390607" MODIFIED="1559025530909" HGAP_QUANTITY="41.749999172985575 pt" VSHIFT_QUANTITY="-5.249999843537813 pt"/>
</node>
<node TEXT="((rat ~) exp1 exp2)" ID="ID_1172107081" CREATED="1558784488317" MODIFIED="1559124547193" HGAP_QUANTITY="499.24998553842346 pt" VSHIFT_QUANTITY="-376.49998877942596 pt">
<node TEXT="(op / simp-exp1 simp-exp2)" ID="ID_1142664484" CREATED="1558784504866" MODIFIED="1559025527319" HGAP_QUANTITY="51.499998882412946 pt" VSHIFT_QUANTITY="22.499999329447764 pt"/>
</node>
<node TEXT="((mlist ~) exp1 exp2 ~)" ID="ID_1744702807" CREATED="1558952492776" MODIFIED="1559124550429" HGAP_QUANTITY="539.7499843314296 pt" VSHIFT_QUANTITY="-374.2499888464812 pt">
<node TEXT="(struct-list simp-exp1 simp-exp2 ~)" ID="ID_1097639564" CREATED="1558952509345" MODIFIED="1559799996906" HGAP_QUANTITY="34.99999937415125 pt" VSHIFT_QUANTITY="8.999999731779106 pt"/>
</node>
<node TEXT="((msetq ~) exp1 exp2)" ID="ID_347493959" CREATED="1558952558533" MODIFIED="1559124557161" HGAP_QUANTITY="556.9999838173394 pt" VSHIFT_QUANTITY="-148.49999557435527 pt">
<node TEXT="(assign simp-exp1 simp-exp2)" ID="ID_1922463434" CREATED="1558952575373" MODIFIED="1559819543985" HGAP_QUANTITY="30.499999508261695 pt" VSHIFT_QUANTITY="11.999999642372142 pt"/>
</node>
<node TEXT="((mdefine simp) ((atom) atom1 atom2 ((mlist) atom3)) exp1)" ID="ID_1996781871" CREATED="1558960518979" MODIFIED="1559286482620" HGAP_QUANTITY="625.249981783331 pt" VSHIFT_QUANTITY="-125.24999626725923 pt">
<node TEXT="(func-def (symbol atom) ((symbol atom1) (symbol atom2) (symbol *atom3)) (body-indented (simp-exp1) ~))" ID="ID_332262275" CREATED="1558960580132" MODIFIED="1559812161672" HGAP_QUANTITY="25.99999964237215 pt" VSHIFT_QUANTITY="-5.249999843537811 pt"/>
</node>
<node TEXT="((mprog/mprogn) exp1 exp2 ~)" ID="ID_627034643" CREATED="1559043055285" MODIFIED="1559477136910" HGAP_QUANTITY="635.7499814704066 pt" VSHIFT_QUANTITY="-95.24999716132888 pt">
<node TEXT="Creation of a function :-&#xa;(func-def (symbol &quot;random&quot;) (&lt;args&gt;) (body-indented &lt;forms&gt;))&#xa;(funcall (symbol &quot;random&quot;) &lt;args&gt;)" ID="ID_1306395358" CREATED="1559043074371" MODIFIED="1559812180693" HGAP_QUANTITY="56.74999872595076 pt" VSHIFT_QUANTITY="1.4999999552965164 pt"/>
</node>
<node TEXT="((%array ~) name num1 num2 ~)" ID="ID_1632833574" CREATED="1559054663325" MODIFIED="1559124565007" HGAP_QUANTITY="622.249981872738 pt" VSHIFT_QUANTITY="-190.49999432265776 pt">
<node TEXT="(asign (SYMBOL &lt;Name&gt;)&#xa;    (OP *&#xa;        (STRUCT-LIST&#xa;            (OP *&#xa;                (STRUCT-LIST ...~... (SYMBOL &quot;None&quot;))&#xa;            num2))&#xa;    num1))" ID="ID_1198638939" CREATED="1559054704470" MODIFIED="1559887219280" HGAP_QUANTITY="67.24999841302638 pt" VSHIFT_QUANTITY="-38.2499988600612 pt"/>
</node>
<node TEXT="((%array ~) name type num1 num2 ~)" ID="ID_642431126" CREATED="1559071201795" MODIFIED="1559124573913" HGAP_QUANTITY="635.7499814704066 pt" VSHIFT_QUANTITY="-213.7499936297538 pt">
<node TEXT="(assign (SYMBOL &lt;Name&gt;)&#xa;    (OP *&#xa;        (STRUCT-LIST&#xa;            (OP *&#xa;                (STRUCT-LIST ...~... (SYMBOL &quot;None&quot;))&#xa;            num2))&#xa;    num1))" ID="ID_1249643074" CREATED="1559071219606" MODIFIED="1559887223145" HGAP_QUANTITY="115.24999698251494 pt" VSHIFT_QUANTITY="-5.999999821186066 pt"/>
</node>
<node TEXT="((%array ~) ((mlist ~) name1 name2 ~) num1 num2 ~)" ID="ID_1954267771" CREATED="1559071312446" MODIFIED="1559124579702" HGAP_QUANTITY="479.7499861195688 pt" VSHIFT_QUANTITY="-296.2499911710622 pt">
<node TEXT="" ID="ID_1822255794" CREATED="1559071362680" MODIFIED="1559071544969">
<node TEXT="(body&#xa;  (assign (SYMBOL &lt;name1&gt;)&#xa;      (OP *&#xa;          (STRUCT-LIST&#xa;                  (OP *&#xa;                      (STRUCT-LIST&#xa;                              (OP * ..~.. (STRUCT-LIST (SYMBOL &quot;None&quot;)) 3))&#xa;                      2))&#xa;          1))&#xa;  (assign (SYMBOL &lt;name2&gt;)&#xa;      (OP *&#xa;          (STRUCT-LIST&#xa;                  (OP *&#xa;                      (STRUCT-LIST&#xa;                              (OP * ..~.. (STRUCT-LIST (SYMBOL &quot;None&quot;)) 3))&#xa;                      2))&#xa;          1))&#xa;&#xa;....&#xa;&#xa;&#xa;  (assign (SYMBOL &lt;name-n&gt;)&#xa;      (OP *&#xa;          (STRUCT-LIST&#xa;                  (OP *&#xa;                      (STRUCT-LIST&#xa;                              (OP * ..~.. (STRUCT-LIST (SYMBOL &quot;None&quot;)) 3))&#xa;                      2))&#xa;          1)))" ID="ID_908587085" CREATED="1559071381657" MODIFIED="1559887246059" HGAP_QUANTITY="62.749998547136826 pt" VSHIFT_QUANTITY="-191.99999427795427 pt">
<node TEXT="(assign (SYMBOL &quot;REF&quot;)" ID="ID_265047219" CREATED="1559071381657" MODIFIED="1559887254495">
<node TEXT="(OP *" ID="ID_1818603375" CREATED="1559071381657" MODIFIED="1559071381657">
<node TEXT="(STRUCT-LIST" ID="ID_594381959" CREATED="1559071381657" MODIFIED="1559887260744">
<node TEXT="(OP *" ID="ID_1005022366" CREATED="1559071381657" MODIFIED="1559071381657">
<node TEXT="(STRUCT-LIST" ID="ID_1180529666" CREATED="1559071381657" MODIFIED="1559887264026">
<node TEXT="(OP * (STRUCT-LIST (SYMBOL &quot;None&quot;)) 3))" ID="ID_620210371" CREATED="1559071381657" MODIFIED="1559887266702"/>
</node>
<node TEXT="2))" ID="ID_1065857604" CREATED="1559071381657" MODIFIED="1559071381657"/>
</node>
</node>
<node TEXT="1))" ID="ID_810258608" CREATED="1559071381657" MODIFIED="1559071381657"/>
</node>
</node>
<node TEXT="(assign (SYMBOL &quot;SVSV&quot;)" ID="ID_875505363" CREATED="1559071381657" MODIFIED="1559887271280">
<node TEXT="(OP *" ID="ID_488550019" CREATED="1559071381657" MODIFIED="1559071381657">
<node TEXT="(STRUCT-LIST" ID="ID_1644520138" CREATED="1559071381657" MODIFIED="1559887275724">
<node TEXT="(OP *" ID="ID_955944066" CREATED="1559071381657" MODIFIED="1559071381657">
<node TEXT="(STRUCT-LIST" ID="ID_746601802" CREATED="1559071381657" MODIFIED="1559887279447">
<node TEXT="(OP * (STRUCT-LIST (SYMBOL &quot;None&quot;)) 3))" ID="ID_1302533987" CREATED="1559071381657" MODIFIED="1559887284376"/>
</node>
<node TEXT="2))" ID="ID_718693418" CREATED="1559071381657" MODIFIED="1559071381657"/>
</node>
</node>
<node TEXT="1))" ID="ID_641712407" CREATED="1559071381658" MODIFIED="1559071381658"/>
</node>
</node>
<node TEXT="(assign (SYMBOL &quot;SDVSDV&quot;)" ID="ID_356734546" CREATED="1559071381658" MODIFIED="1559887290694">
<node TEXT="(OP *" ID="ID_103062853" CREATED="1559071381658" MODIFIED="1559071381658">
<node TEXT="(STRUCT-LIST" ID="ID_119986940" CREATED="1559071381658" MODIFIED="1559887293372">
<node TEXT="(OP *" ID="ID_128579023" CREATED="1559071381658" MODIFIED="1559071381658">
<node TEXT="(STRUCT-LIST" ID="ID_368785290" CREATED="1559071381658" MODIFIED="1559887296158">
<node TEXT="(OP * (STRUCT-LIST (SYMBOL &quot;None&quot;)) 3))" ID="ID_919826391" CREATED="1559071381658" MODIFIED="1559887304365"/>
</node>
<node TEXT="2))" ID="ID_88196448" CREATED="1559071381658" MODIFIED="1559071381658"/>
</node>
</node>
<node TEXT="1)))" ID="ID_141447378" CREATED="1559071381658" MODIFIED="1559071381658"/>
</node>
</node>
</node>
</node>
</node>
<node TEXT="((array-name simp array) index1 index2 ~_" ID="ID_1043136972" CREATED="1559124263014" MODIFIED="1559125098949" HGAP_QUANTITY="-1478.4999555200352 pt" VSHIFT_QUANTITY="-3148.4999061673907 pt">
<node TEXT="(element-array (element-array (element-array (symbol &lt;array-name&gt;) index-1) index-2) .. index-n)" ID="ID_92049242" CREATED="1559124292922" MODIFIED="1559124431826" HGAP_QUANTITY="100.99999740719802 pt" VSHIFT_QUANTITY="-5.999999821186071 pt"/>
</node>
<node TEXT="DEFAULT CASE" ID="ID_1261824911" CREATED="1559126230730" MODIFIED="1559126247684">
<node TEXT="(funcall (symbol &lt;(caar form)&gt;) simp-exp1 simp-exp2 ~)" ID="ID_573367386" CREATED="1559126247689" MODIFIED="1559126280177"/>
</node>
<node TEXT="((mcond) cond1 exp1 cond2 exp2 ... condn expn t expnn)" ID="ID_379357604" CREATED="1559484409265" MODIFIED="1559484414436">
<node TEXT="(conditional cond1 exp1 (conditional cond2 exp2 expnn))" ID="ID_1806861198" CREATED="1559484414442" MODIFIED="1559484461079"/>
</node>
</node>
<node TEXT="((mnot ~) exp)" ID="ID_274846077" CREATED="1559143884473" MODIFIED="1559205465566" HGAP_QUANTITY="112.99999704957017 pt" VSHIFT_QUANTITY="-494.9999852478509 pt">
<node TEXT="(funcall (symbol &quot;not&quot;) simp-exp)" ID="ID_495648796" CREATED="1559143901022" MODIFIED="1559143925740"/>
</node>
<node TEXT="((mand ~) exp-1 exp-2 ~)" ID="ID_208312487" CREATED="1559143929911" MODIFIED="1559143954979">
<node TEXT="(boolop and simp-exp-1 simp-exp-2)" ID="ID_1832522343" CREATED="1559143954984" MODIFIED="1559143976890"/>
</node>
<node TEXT="((mor ~) exp-1 exp-2 ~)" ID="ID_137775565" CREATED="1559143978104" MODIFIED="1559143986509">
<node TEXT="(boolop or simp-exp-1 simp-exp-2)" ID="ID_1150268404" CREATED="1559143986513" MODIFIED="1559144177723"/>
</node>
<node TEXT="((mgreaterp ~) exp exp)" ID="ID_1767493904" CREATED="1559153568836" MODIFIED="1559153651726">
<node TEXT="(comp-op &gt; exp exp)" ID="ID_1639028840" CREATED="1559153941443" MODIFIED="1559888115626"/>
</node>
<node TEXT="((mequal ~) exp exp)" ID="ID_159736405" CREATED="1559153655570" MODIFIED="1559153662377">
<node TEXT="(comp-op == exp exp)" ID="ID_1487374525" CREATED="1559153976355" MODIFIED="1559888119509"/>
</node>
<node TEXT="((mnotequal ~) exp exp)" ID="ID_1667214423" CREATED="1559153662916" MODIFIED="1559153669411">
<node TEXT="(comp-op != exp exp)" ID="ID_145398949" CREATED="1559153986681" MODIFIED="1559888123803"/>
</node>
<node TEXT="((mlessp ~) exp exp)" ID="ID_550415132" CREATED="1559153669803" MODIFIED="1559153677071">
<node TEXT="(comp-op &lt; exp exp)" ID="ID_1974197226" CREATED="1559153996392" MODIFIED="1559888130547"/>
</node>
<node TEXT="((mgeqp ~) exp exp)" ID="ID_1220781504" CREATED="1559153678706" MODIFIED="1559205519900" HGAP_QUANTITY="-219.99999302625676 pt" VSHIFT_QUANTITY="-1136.9999661147604 pt">
<node TEXT="(comp-op &gt;= exp exp)" ID="ID_403486097" CREATED="1559154014492" MODIFIED="1559888134791"/>
</node>
<node TEXT="((mleqp ~) exp exp)" ID="ID_1009687600" CREATED="1559153583529" MODIFIED="1559205478235" HGAP_QUANTITY="367.99998944997816 pt" VSHIFT_QUANTITY="-1979.9999409914035 pt">
<node TEXT="(comp-op &lt;= exp exp)" ID="ID_729342354" CREATED="1559154026017" MODIFIED="1559888138095"/>
</node>
<node TEXT="((lambda ~) ((mlist) s1 s2 ~) exp-1)" FOLDED="true" ID="ID_1159602754" CREATED="1560499082077" MODIFIED="1560499157293">
<node TEXT="(lambda (simp-s1 simp-s2) simp-exp-1)" ID="ID_299579021" CREATED="1560499157304" MODIFIED="1560499175932"/>
</node>
<node TEXT="((lambda ~) ((mlist) s1 s2 ~) exp-1 exp-2 ~)" ID="ID_753289066" CREATED="1560534259607" MODIFIED="1560534273623">
<node TEXT="(BODY (FUNC-DEF (SYMBOL &quot;func19167&quot;) NIL&#xa;          (BODY-INDENTED simp-exp1&#xa;              simp-exp2&#xa;              (FUNCALL (SYMBOL &quot;return&quot;)&#xa;                       (OP / (SYMBOL &quot;a&quot;) (SYMBOL &quot;b&quot;)))))&#xa;(LAMBDA () (FUNCALL (SYMBOL &quot;func19167&quot;)))" ID="ID_1668254200" CREATED="1560534273628" MODIFIED="1560535785478"/>
</node>
<node TEXT="((MDOIN SIMP) $I ((MLIST SIMP)) NIL NIL NIL NIL (($PRINT SIMP) $I)" FOLDED="true" ID="ID_241512391" CREATED="1560758563567" MODIFIED="1560758615849">
<node TEXT="(for-list (symbol &quot;i&quot;) (struct-list) (body-indented exps)" ID="ID_458267467" CREATED="1560758615993" MODIFIED="1560758674859"/>
</node>
<node TEXT="((MDO SIMP) $I 10 5 NIL NIL ((MLEQP SIMP) ((MTIMES SIMP) 5 $I) 30)&#xa; ((MPROGN SIMP) (($PRINT SIMP) $I)&#xa;  (($PRINT SIMP) ((MTIMES SIMP) 5 $I))))" ID="ID_1381954472" CREATED="1560883665500" MODIFIED="1560883674386">
<node TEXT="(BODY (BODY (ASSIGN (SYMBOL &quot;i&quot;) (NUM 10 0))&#xa;            (WHILE-LOOP&#xa;                (FUNCALL (SYMBOL &quot;not&quot;)&#xa;                         (COMP-OP &lt;= (OP * (NUM 5 0) (SYMBOL &quot;i&quot;))&#xa;                                  (NUM 30 0)))&#xa;                (BODY-INDENTED (FUNCALL (SYMBOL &quot;print&quot;) (SYMBOL &quot;i&quot;))&#xa;                    (FUNCALL (SYMBOL &quot;print&quot;)&#xa;                             (OP * (NUM 5 0) (SYMBOL &quot;i&quot;)))&#xa;                    (ASSIGN (SYMBOL &quot;i&quot;) (OP + (SYMBOL &quot;i&quot;) (NUM 5 0)))))&#xa;            (DEL (SYMBOL &quot;i&quot;))))" ID="ID_993746514" CREATED="1560883674433" MODIFIED="1560883692204"/>
</node>
</node>
</node>
</node>
</node>
</map>
