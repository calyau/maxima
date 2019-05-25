<map version="freeplane 1.7.0">
<!--To view this file, download free mind mapping software Freeplane from http://freeplane.sourceforge.net -->
<node TEXT="maxima-to-ir" FOLDED="false" ID="ID_358261271" CREATED="1558686007510" MODIFIED="1558784738482" STYLE="oval">
<font SIZE="18"/>
<hook NAME="MapStyle">
    <properties edgeColorConfiguration="#808080ff,#ff0000ff,#0000ffff,#00ff00ff,#ff00ffff,#00ffffff,#7c0000ff,#00007cff,#007c00ff,#7c007cff,#007c7cff,#7c7c00ff" fit_to_viewport="false" show_icon_for_attributes="true" show_note_icons="true"/>

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
<node TEXT="atom" POSITION="right" ID="ID_1798541669" CREATED="1558686103789" MODIFIED="1558686111106">
<edge COLOR="#ff0000"/>
<node TEXT="$%i" ID="ID_425784385" CREATED="1558686112963" MODIFIED="1558686121485">
<node TEXT="(num 0 1)" ID="ID_981644649" CREATED="1558686121493" MODIFIED="1558686132850"/>
</node>
<node TEXT="$%pi" ID="ID_1170399689" CREATED="1558763346503" MODIFIED="1558763358727">
<node TEXT="(num pi 0)" ID="ID_360510773" CREATED="1558763358738" MODIFIED="1558763367530"/>
</node>
<node TEXT="$%e" ID="ID_1397422185" CREATED="1558763399655" MODIFIED="1558763405351">
<node TEXT="(num e 0)" ID="ID_1992735641" CREATED="1558763410422" MODIFIED="1558763415024"/>
</node>
</node>
<node TEXT="(consp form)" POSITION="left" ID="ID_1348542721" CREATED="1558783725856" MODIFIED="1558784738480" HGAP_QUANTITY="20.74999979883433 pt" VSHIFT_QUANTITY="-33.74999899417165 pt">
<edge COLOR="#0000ff"/>
<node TEXT="(consp (car form))" ID="ID_238191698" CREATED="1558783980523" MODIFIED="1558784732809" HGAP_QUANTITY="13.250000022351742 pt" VSHIFT_QUANTITY="-21.749999351799506 pt">
<node TEXT="((atom ~) ~)" ID="ID_1483328184" CREATED="1558783994895" MODIFIED="1558784729317" HGAP_QUANTITY="12.500000044703482 pt" VSHIFT_QUANTITY="-32.24999903887513 pt">
<node TEXT="(atom (caar form))" ID="ID_1347493343" CREATED="1558784008079" MODIFIED="1558784768590" HGAP_QUANTITY="8.000000178813929 pt" VSHIFT_QUANTITY="-42.74999872595075 pt">
<node TEXT="((mtimes ~) exp1 exp2 ~)" ID="ID_1425557450" CREATED="1558784069181" MODIFIED="1558784760050" HGAP_QUANTITY="39.4999992400408 pt" VSHIFT_QUANTITY="-5.249999843537812 pt">
<node TEXT="(op * simp-exp1 simp-exp2 ~)" ID="ID_1877074756" CREATED="1558784151010" MODIFIED="1558784745508" HGAP_QUANTITY="11.750000067055222 pt" VSHIFT_QUANTITY="-25.4999992400408 pt"/>
</node>
<node TEXT="((mplus ~) exp1 exp2 ~)" ID="ID_577061991" CREATED="1558784193235" MODIFIED="1558784762363" HGAP_QUANTITY="41.74999917298558 pt" VSHIFT_QUANTITY="-6.74999979883433 pt">
<node TEXT="(op + simp-exp1 simp-exp2 ~)" ID="ID_1681649521" CREATED="1558784240938" MODIFIED="1558784747706" HGAP_QUANTITY="33.49999941885473 pt" VSHIFT_QUANTITY="-17.99999946355821 pt"/>
</node>
<node TEXT="((mexpt ~) exp1 exp2)" ID="ID_1366568193" CREATED="1558784216863" MODIFIED="1558784765182" HGAP_QUANTITY="41.74999917298558 pt" VSHIFT_QUANTITY="-0.7499999776482589 pt">
<node TEXT="(funcall pow simp-exp1 simp-exp2)" ID="ID_1195580292" CREATED="1558784358428" MODIFIED="1558784750267" HGAP_QUANTITY="52.249998860061204 pt" VSHIFT_QUANTITY="-1.4999999552965178 pt"/>
</node>
<node TEXT="((mfactorial ~) exp1)" ID="ID_84710372" CREATED="1558784291690" MODIFIED="1558784768589" HGAP_QUANTITY="48.499998971819906 pt" VSHIFT_QUANTITY="14.249999575316918 pt">
<node TEXT="(funcall math.factorial simp-exp1)" ID="ID_1412481236" CREATED="1558784390607" MODIFIED="1558784753611" HGAP_QUANTITY="43.2499991282821 pt" VSHIFT_QUANTITY="24.749999262392542 pt"/>
</node>
<node TEXT="((rat ~) exp1 exp2)" ID="ID_1172107081" CREATED="1558784488317" MODIFIED="1558784757545">
<node TEXT="(op / simp-exp1 simp-exp2)" ID="ID_1142664484" CREATED="1558784504866" MODIFIED="1558784757543" HGAP_QUANTITY="60.49999861419205 pt" VSHIFT_QUANTITY="29.249999128282095 pt"/>
</node>
</node>
</node>
</node>
</node>
</node>
</map>
