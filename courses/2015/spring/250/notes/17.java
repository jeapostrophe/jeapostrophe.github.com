import java.util.Random;

class C17 {
    static double sign(double x) {
        if (x < 0.0) return -1;
        if (x > 0.0) return +1;
        return 0.0;
    }

    static double a = 0.140012;
    static double inv_erf(double x) {
        return sign(x) *
            Math.sqrt
            (Math.sqrt
             (Math.pow
              (2 / Math.PI * a +
               Math.log(1 - Math.pow(x,2))/2,
               2) -
              Math.log(1 - Math.pow(x,2))/a)
             - ((2/(Math.PI*a)) +
                Math.log(1 - Math.pow(x,2))/2));
    }

    static double inv_phi(double p) {
        return Math.sqrt(2.0) * inv_erf(2.0 * p - 1);
    }

    static double normal_inv_cdf(double mu, double sig, double p) {
        return mu + sig * inv_phi(p);
    }

    public static void main (String[] args) {
        Random r = new Random();

        double mu = 0.0;
        double sig = 1.0;

        System.out.println("(");
        for (int i = 0; i < 10000; i++ ) {
            // u1 & u2 is uniform on [0, 1]
            double u1 = 0.0;
            while (u1 == 0.0) {
                u1 = r.nextDouble();
            }
            double u2 = r.nextDouble();

            // wn is uniform on [-5, 5]
            double wn = u1 * 10.0 - 5.0;

            // n is normal
            double n = normal_inv_cdf(mu, sig, u1);

            // box-muller
            double x =
                  Math.sqrt(-2.0 * Math.log(u1))
                *  Math.sin(+2.0 * Math.PI * u2);
            double bm = mu + sig * x;
    
            System.out.println(bm);
        }
        System.out.println(")");
    }
}
