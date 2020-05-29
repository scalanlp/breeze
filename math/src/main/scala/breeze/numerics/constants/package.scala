package breeze.numerics

/**This package specifies standard numerical/scientific constants in SI units.
 *
 * @author ktakagaki * @date 3/13/14.
 */
package object constants {

  // <editor-fold defaultstate="collapsed" desc=" Mathematics ">

  /** ALIAS FOR [[scala.math.Pi]].
   * @see <a href="http://en.wikipedia.org/wiki/Pi">http://en.wikipedia.org/wiki/Pi</a>
   */
  lazy val Pi = scala.math.Pi

  lazy val π = Pi

  /** ALIAS FOR [[scala.math.E]].
   * @see <a href="http://en.wikipedia.org/wiki/%E2%84%AF">http://en.wikipedia.org/wiki/%E2%84%AF</a>
   */
  lazy val E = scala.math.E

  /** (1 + sqrt(5))/2
   * @see <a href="http://en.wikipedia.org/wiki/Golden_ratio">http://en.wikipedia.org/wiki/Golden_ratio</a>
   */
  lazy val GoldenRatio: Double = (Math.sqrt(5d) + 1d) / 2d

  /**
   * http://en.wikipedia.org/wiki/Euler%E2%80%93Mascheroni_constant
   * (value from wikipedia)
   */
  lazy val eulerMascheroni = 0.5772156649015328606065120900824024310421

  lazy val γ = eulerMascheroni

  // </editor-fold>

  // <editor-fold defaultstate="collapsed" desc=" Electromagnetism ">

  /** [N / A2]
   *@see <a href="http://physics.nist.gov/cgi-bin/cuu/Value?mu0">http://physics.nist.gov/cgi-bin/cuu/Value?mu0</a>
   */
  val MagneticConstant: Double = 4.0e-7 * Pi

  /** Alias for [[MagneticConstant]]
   */
  lazy val Mu0: Double = MagneticConstant

  /** [F / m]
   *@see <a href="http://physics.nist.gov/cgi-bin/cuu/Value?ep0">http://physics.nist.gov/cgi-bin/cuu/Value?ep0</a>
   */
  val ElectricConstant: Double = 8.854187817e-12

  /** Alias for [[ElectricConstant]]
   */
  lazy val Epsilon0: Double = ElectricConstant

  /** []
   *@see <a href="http://physics.nist.gov/cgi-bin/cuu/Value?alph">http://physics.nist.gov/cgi-bin/cuu/Value?alph</a>
   */
  val FineStructureConstant: Double = 7.2973525698e-3

  /** Alias for [[FineStructureConstant]]
   */
  lazy val Alpha: Double = FineStructureConstant

  // </editor-fold>

  // <editor-fold defaultstate="collapsed" desc=" Mechanics ">

  /** [m3 /kg /s]
   * @see <a href="http://physics.nist.gov/cgi-bin/cuu/Value?bg">http://physics.nist.gov/cgi-bin/cuu/Value?bg</a>
   */
  val GravitationConstant: Double = 6.67384e-11

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
   * @see <a href="http://physics.nist.gov/cgi-bin/cuu/Value?na">http://physics.nist.gov/cgi-bin/cuu/Value?na</a>
   */
  val AvogadroNumber: Double = 6.02214129e23

  /** ALIAS FOR [[AvogadroNumber]]
   */
  lazy val NA = AvogadroNumber

  /** [J /mol /K]
   * @see <a href="http://physics.nist.gov/cgi-bin/cuu/Value?r">http://physics.nist.gov/cgi-bin/cuu/Value?r</a>
   */
  val MolarGasConstant: Double = 8.3144621

  /** ALIAS FOR [[Constants.MolarGasConstant]]
   */
  lazy val R: Double = MolarGasConstant

  /** [J /K]
   * @see <a href="http://physics.nist.gov/cgi-bin/cuu/Value?k">http://physics.nist.gov/cgi-bin/cuu/Value?k</a>
   */
  val BoltzmannConstant: Double = 1.3806488e-23

  /** ALIAS FOR [[BoltzmannConstant]]
   */
  lazy val k = BoltzmannConstant

  // </editor-fold>

  // <editor-fold defaultstate="collapsed" desc=" Particle Physics ">
  /** [m/s]
   * @see <a href="http://en.wikipedia.org/wiki/Light_speed">http://en.wikipedia.org/wiki/Light_speed</a>
   */
  val LightSpeed: Double = 2.99792458e8

  /** ALIAS FOR [[LightSpeed]]
   */
  lazy val c = LightSpeed

  /** [J * s]
   * @see <a href="http://physics.nist.gov/cgi-bin/cuu/Value?h">http://physics.nist.gov/cgi-bin/cuu/Value?h</a>
   */
  val PlanckConstant: Double = 6.62606957e-34

  /** ALIAS FOR [[PlanckConstant]]
   */
  lazy val h = PlanckConstant

  /** ALIAS FOR [[PlanckConstant]]/(2Pi)
   */
  lazy val hBar = PlanckConstant / 2d / Pi

  /** [C]
   * @see <a href="http://physics.nist.gov/cgi-bin/cuu/Value?e">http://physics.nist.gov/cgi-bin/cuu/Value?e</a>
   */
  val ElementaryCharge: Double = 1.602176565e-19

  /** ALIAS FOR [[ElementaryCharge]]
   */
  lazy val e = ElementaryCharge

  /** [kg]
   * @see <a href="http://physics.nist.gov/cgi-bin/cuu/Value?me">http://physics.nist.gov/cgi-bin/cuu/Value?me</a>
   */
  val ElectronMass: Double = 9.10938291e-31

  /** [kg]
   * @see <a href="http://physics.nist.gov/cgi-bin/cuu/Value?mp">http://physics.nist.gov/cgi-bin/cuu/Value?mp</a>
   */
  val ProtonMass: Double = 1.672621777e-27

  /** [kg]
   * @see <a href="http://physics.nist.gov/cgi-bin/cuu/Value?mn">http://physics.nist.gov/cgi-bin/cuu/Value?mn</a>
   */
  val NeutronMass: Double = 1.674927351e-27

  /** [/m]
   * @see <a href="http://physics.nist.gov/cgi-bin/cuu/Value?ryd">http://physics.nist.gov/cgi-bin/cuu/Value?ryd</a>
   */
  val RydbergConstant: Double = 10973731.568539

  /** [m K]
   * @see <a href="http://physics.nist.gov/cgi-bin/cuu/Value?bwien">http://physics.nist.gov/cgi-bin/cuu/Value?bwien</a>
   */
  val WienDisplacementLawConstant: Double = 2.8977721e-3

  /** ALIAS FOR [[WienDisplacementLawConstant]]
   */
  lazy val Wien = WienDisplacementLawConstant

  /** [W /m2 /K4]
   * @see <a href="http://physics.nist.gov/cgi-bin/cuu/Value?sigma">http://physics.nist.gov/cgi-bin/cuu/Value?sigma</a>
   */
  val StefanBoltzmannConstant: Double = 5.670373e-8

  /** ALIAS FOR [[StefanBoltzmannConstant]]
   */
  lazy val sigma = StefanBoltzmannConstant

  // </editor-fold>

  // <editor-fold defaultstate="collapsed" desc=" SI Prefixes ">

  /**SI prefix for 1.0E24*/
  val yotta = 1.0e24

  /**SI prefix for 1.0E21*/
  val zetta = 1.0e21

  /**SI prefix for 1.0E18*/
  val exa = 1.0e18

  /**SI prefix for 1.0E15*/
  val peta = 1.0e15

  /**SI prefix for 1.0E12*/
  val tera = 1.0e12

  /**SI prefix for 1.0E9*/
  val giga = 1.0e9

  /**SI prefix for 1.0E6*/
  val mega = 1.0e6

  /**SI prefix for 1.0E3*/
  val kilo = 1.0e3

  /**SI prefix for 1.0E2*/
  val hecto = 1.0e2

  /**SI prefix for 1.0E1*/
  val deca = 1.0e1

  /**SI prefix for 1.0E-1*/
  val deci = 1.0e-1

  /**SI prefix for 1.0E-2*/
  val centi = 1.0e-2

  /**SI prefix for 1.0E-3*/
  val milli = 1.0e-3

  /**SI prefix for 1.0E-6*/
  val micro = 1.0e-6

  /**SI prefix for 1.0E-9*/
  val nano = 1.0e-9

  /**SI prefix for 1.0E-12*/
  val pico = 1.0e-12

  /**SI prefix for 1.0E-15*/
  val femto = 1.0e-15

  /**SI prefix for 1.0E-18*/
  val atto = 1.0e-18

  /**SI prefix for 1.0E-21*/
  val zepto = 1.0e-21

  /**SI prefix for 1.0E-24*/
  val yocto = 1.0e-24

  // </editor-fold>
  // <editor-fold defaultstate="collapsed" desc=" Binary Prefixes ">

  /**Binary prefix for pow(2, 10)*/
  val kibi = pow(2.0, 10d)

  /**Binary prefix for pow(2, 20)*/
  val mebi = pow(2.0, 20d)

  /**Binary prefix for pow(2, 30)*/
  val gibi = pow(2.0, 30d)

  /**Binary prefix for pow(2, 40)*/
  val tebi = pow(2.0, 40d)

  /**Binary prefix for pow(2, 50)*/
  val pebi = pow(2.0, 50d)

  /**Binary prefix for pow(2, 60)*/
  val exbi = pow(2.0, 60d)

  /**Binary prefix for pow(2, 70)*/
  val zebi = pow(2.0, 70d)

  /**Binary prefix for pow(2, 80)*/
  val yobi = pow(2.0, 80d)

  // </editor-fold>

}
