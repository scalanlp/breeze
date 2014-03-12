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

  // <editor-fold defaultstate="collapsed" desc=" Electromagnetism ">

  /** [N / A2]
    *@see <a href="http://physics.nist.gov/cgi-bin/cuu/Value?mu0">http://physics.nist.gov/cgi-bin/cuu/Value?mu0</a>
    */
  val MagneticConstant: Double = 4.0E-7 * Pi
  /** Alias for [[MagneticConstant]]
    */
  val Mu0: Double = MagneticConstant

  /** [F / m]
    *@see <a href="http://physics.nist.gov/cgi-bin/cuu/Value?ep0">http://physics.nist.gov/cgi-bin/cuu/Value?ep0</a>
    */
  val ElectricConstant: Double = 8.854187817E-12
  /** Alias for [[ElectricConstant]]
    */
  val Epsilon0: Double = ElectricConstant

  /** []
    *@see <a href="http://physics.nist.gov/cgi-bin/cuu/Value?alph">http://physics.nist.gov/cgi-bin/cuu/Value?alph</a>
    */
  val FineStructureConstant: Double = 7.2973525698E-3
  /** Alias for [[FineStructureConstant]]
    */
  val Alpha: Double = FineStructureConstant

  // </editor-fold>

  // <editor-fold defaultstate="collapsed" desc=" Mechanics ">

  /** [m3 /kg /s]
    * @see <a href="http://en.wikipedia.org/wiki/Gravitation_constant">http://en.wikipedia.org/wiki/Gravitation_constant</a>
    */
  val GravitationConstant: Double = 6.67384E-11
  /** [m /s2]
    * @see <a href="http://physics.nist.gov/cgi-bin/cuu/Value?gn">http://physics.nist.gov/cgi-bin/cuu/Value?gn</a>
    */
  val StandardAccelerationOfGravity: Double = 9.80665
  /** ALIAS FOR [[StandardAccelerationOfGravity]]
    */
  val g = StandardAccelerationOfGravity

  // </editor-fold>

  // <editor-fold defaultstate="collapsed" desc=" Thermodynamics ">

  /** [ /mol]
    * @see <a href="http://en.wikipedia.org/wiki/Avogadro_number">http://en.wikipedia.org/wiki/Avogadro_number</a>
    */
  val AvogadroNumber: Double = 6.02214129E23
  /** ALIAS FOR [[AvogadroNumber]]
    */
  val NA = AvogadroNumber

  /** [J /mol /K]
    * @see <a href="http://en.wikipedia.org/wiki/Molar_gas_constant">http://en.wikipedia.org/wiki/Molar_gas_constant</a>
    */
  val MolarGasConstant: Double = 8.3144621
  /** ALIAS FOR [[breeze.numerics.Constants.MolarGasConstant]]
    */
  val R: Double = MolarGasConstant

  /** [J /K]
    * @see <a href="http://physics.nist.gov/cgi-bin/cuu/Value?k">http://physics.nist.gov/cgi-bin/cuu/Value?k</a>
    */
  val BoltzmannConstant: Double = 1.3806488E-23
  /** ALIAS FOR [[BoltzmannConstant]]
    */
  val k = BoltzmannConstant

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
  /** ALIAS FOR [[PlanckConstant]]
    */
  val h = PlanckConstant
  /** ALIAS FOR [[PlanckConstant]]/(2Pi)
    */
  val hBar = PlanckConstant / 2d / Pi

  /** [C]
    * @see <a href="http://en.wikipedia.org/wiki/Electron_charge">http://en.wikipedia.org/wiki/Electron_charge</a>
    */
  val ElementaryCharge: Double = 1.602176565E-19
  /** ALIAS FOR [[ElementaryCharge]]
    */
  val e = ElementaryCharge

  /** [kg]
    * @see <a href="http://physics.nist.gov/cgi-bin/cuu/Value?me">http://physics.nist.gov/cgi-bin/cuu/Value?me</a>
    */
  val ElectronMass: Double = 9.10938291E-31

  /** [kg]
    * @see <a href="http://physics.nist.gov/cgi-bin/cuu/Value?mp">http://physics.nist.gov/cgi-bin/cuu/Value?mp</a>
    */
  val ProtonMass: Double = 1.672621777E-27

  /** [kg]
    * @see <a href="http://physics.nist.gov/cgi-bin/cuu/Value?mn">http://physics.nist.gov/cgi-bin/cuu/Value?mn</a>
    */
  val NeutronMass: Double = 1.674927351E-27

  /** [/m]
    * @see <a href="http://physics.nist.gov/cgi-bin/cuu/Value?ryd">http://physics.nist.gov/cgi-bin/cuu/Value?ryd</a>
    */
  val RydbergConstant: Double = 10973731.568539

  /** [m K]
    * @see <a href="http://physics.nist.gov/cgi-bin/cuu/Value?bwien">http://physics.nist.gov/cgi-bin/cuu/Value?bwien</a>
    */
  val WienDisplacementLawConstant: Double = 2.8977721E-3
  /** ALIAS FOR [[WienDisplacementLawConstant]]
    */
  val Wien = WienDisplacementLawConstant


  /** [W /m2 /K4]
    * @see <a href="http://physics.nist.gov/cgi-bin/cuu/Value?bwien">http://physics.nist.gov/cgi-bin/cuu/Value?bwien</a>
    */
  val StefanBoltzmannConstant: Double = 5.670373E-8
  /** ALIAS FOR [[StefanBoltzmannConstant]]
    */
  val sigma = StefanBoltzmannConstant


  // </editor-fold>


}
