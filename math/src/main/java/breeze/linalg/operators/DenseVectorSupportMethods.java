package breeze.linalg.operators;

/**
 * Created by dlwh on 8/14/15.
 *
 * Created because I need fallthrough
 */
final public class DenseVectorSupportMethods {

    public static final int MAX_SMALL_DOT_PRODUCT_LENGTH = 8;

    /**
     * WARNING: only returns the right answer for vectors of length MAX_SMALL_DOT_PRODUCT_LENGTH or less
     * @param a
     * @param b
     * @param length
     * @return
     */
    public static double smallDotProduct_Double(double[] a, double[] b, int length) {
        double sumA = 0.0;
        double sumB = 0.0;
        switch (length) {
            case 8:
                sumB = a[7] * b[7];
            case 7:
                sumA = a[6] * b[6];
            case 6:
                sumB += a[5] * b[5];
            case 5:
                sumA += a[4] * b[4];
            case 4:
                sumB += a[3] * b[3];
            case 3:
                sumA += a[2] * b[2];
            case 2:
                sumB += a[1] * b[1];
            case 1:
                sumA += a[0] * b[0];
            case 0:
            default:
                return sumA + sumB;
        }
    }

    public static final int UNROLL_LENGTH = 8;

    public static double dotProduct_Double(double[] a, int aoff, double[] b, int boff, int length) {
        int extra = length % UNROLL_LENGTH;
        int loops = length / UNROLL_LENGTH;
        double sum0 = 0.0;
        double sum1 = 0.0;
        double sum2 = 0.0;
        double sum3 = 0.0;
        double sum4 = 0.0;
        double sum5 = 0.0;
        double sum6 = 0.0;
        double sum7 = 0.0;

        for(int i = 0; i < extra; i++) {
            sum0 += a[aoff + i] * b[boff + i];
        }
        aoff += extra;
        boff += extra;

        for(int i = 0; i < loops; i++, aoff += UNROLL_LENGTH, boff += UNROLL_LENGTH) {
            sum0 += a[aoff + 0] * b[boff + 0];
            sum1 += a[aoff + 1] * b[boff + 1];
            sum2 += a[aoff + 2] * b[boff + 2];
            sum3 += a[aoff + 3] * b[boff + 3];
            sum4 += a[aoff + 4] * b[boff + 4];
            sum5 += a[aoff + 5] * b[boff + 5];
            sum6 += a[aoff + 6] * b[boff + 6];
            sum7 += a[aoff + 7] * b[boff + 7];
        }

        return sum0 + sum1 + sum2 + sum3 + sum4 + sum5 + sum6 + sum7;
    }


    /**
     * WARNING: only returns the right answer for vectors of length MAX_SMALL_DOT_PRODUCT_LENGTH or less
     * @param a
     * @param b
     * @param length
     * @return
     */
    public static float smallDotProduct_Float(float[] a, float[] b, int length) {
        float sum = 0.0f;
        switch (length) {
            case 6:
                sum += a[5] * b[5];
            case 5:
                sum += a[4] * b[4];
            case 4:
                sum += a[3] * b[3];
            case 3:
                sum += a[2] * b[2];
            case 2:
                sum += a[1] * b[1];
            case 1:
                sum += a[0] * b[0];
            case 0:
            default:
                return sum;
        }
    }

    public static float dotProduct_Float(float[] a, int aoff, float[] b, int boff, int length) {
        int extra = length % UNROLL_LENGTH;
        int loops = length / UNROLL_LENGTH;
        float sum0 = 0.0f;
        float sum1 = 0.0f;
        float sum2 = 0.0f;
        float sum3 = 0.0f;
        float sum4 = 0.0f;
        float sum5 = 0.0f;
        float sum6 = 0.0f;
        float sum7 = 0.0f;

        for(int i = 0; i < extra; i++) {
            sum0 += a[aoff + i] * b[boff + i];
        }
        aoff += extra;
        boff += extra;

        for(int i = 0; i < loops; i++, aoff += UNROLL_LENGTH, boff += UNROLL_LENGTH) {
            sum0 += a[aoff + 0] * b[boff + 0];
            sum1 += a[aoff + 1] * b[boff + 1];
            sum2 += a[aoff + 2] * b[boff + 2];
            sum3 += a[aoff + 3] * b[boff + 3];
            sum4 += a[aoff + 4] * b[boff + 4];
            sum5 += a[aoff + 5] * b[boff + 5];
            sum6 += a[aoff + 6] * b[boff + 6];
            sum7 += a[aoff + 7] * b[boff + 7];
        }

        return sum0 + sum1 + sum2 + sum3 + sum4 + sum5 + sum6 + sum7;
    }
}
