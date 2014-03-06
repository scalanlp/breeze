package breeze.numerics

/**This package specifies standard numerical/scientific constants.
 * SI/MKS units where applicable, specified in name if otherwise.
  *
 * @author ktakagaki
 * @date 1/31/14.
 */
object Constants {

  // <editor-fold defaultstate="collapsed" desc=" Mathematics ">

  /** ALIAS FOR [[scala.math.Pi]].
    * @see <a href="http://en.wikipedia.org/wiki/Pi">http://en.wikipedia.org/wiki/Pi</a>
    */
  val Pi = scala.math.Pi

  /** ALIAS FOR [[scala.math.E]].
    * @see <a href="http://en.wikipedia.org/wiki/%E2%84%AF">http://en.wikipedia.org/wiki/%E2%84%AF</a>
    */
  val E = scala.math.E

  /** (1 + sqrt(5))/2
    * @see <a href="http://en.wikipedia.org/wiki/Golden_ratio">http://en.wikipedia.org/wiki/Golden_ratio</a>
    */
  val GoldenRatio: Double = (Math.sqrt(5d) + 1d)/2d

  // </editor-fold>

  // <editor-fold defaultstate="collapsed" desc=" Mechanics ">

  /** [m3 /kg /s]
    * @see <a href="http://en.wikipedia.org/wiki/Gravitation_constant">http://en.wikipedia.org/wiki/Gravitation_constant</a>
    */
  val GravitationConstant: Double = 6.67384E-11

  // </editor-fold>

  // <editor-fold defaultstate="collapsed" desc=" Thermodynamics ">

  /** [ /mol]
    * @see <a href="http://en.wikipedia.org/wiki/Avogadro_number">http://en.wikipedia.org/wiki/Avogadro_number</a>
    */
  val AvogadroNumber: Double = 6.02214129E23

  /** [J /mol /K]
    * @see <a href="http://en.wikipedia.org/wiki/Molar_gas_constant">http://en.wikipedia.org/wiki/Molar_gas_constant</a>
    */
  val MolarGasConstant: Double = 8.3144621
  /** ALIAS FOR [[breeze.numerics.Constants.MolarGasConstant]] [J / mol * K]
    */
  val R: Double = MolarGasConstant

  // </editor-fold>

  // <editor-fold defaultstate="collapsed" desc=" Particle Physics ">
  /** [m/s]
    * @see <a href="http://en.wikipedia.org/wiki/Light_speed">http://en.wikipedia.org/wiki/Light_speed</a>
    */
  val LightSpeed: Double = 2.99792458E8

  /** [J * s]
    * @see <a href="http://en.wikipedia.org/wiki/Planck_Constant">http://en.wikipedia.org/wiki/Planck_Constant</a>
    */
  val PlanckConstant: Double = 6.62606957E-34

  /** [C]
    * @see <a href="http://en.wikipedia.org/wiki/Electron_charge">http://en.wikipedia.org/wiki/Electron_charge</a>
    */
  val ElementaryCharge: Double = 1.602176565E-19


  // </editor-fold>


}
