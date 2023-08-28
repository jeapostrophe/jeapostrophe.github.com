import java.util.Random;

class C15 {
    // f(x) = x^2 + 40 * sin(x)
    static double f ( double x ) {
        return x * x + 40 * Math.sin(x);
    }
    // f'(x) = 2x + 40 * cos(x)
    static double fp ( double x ) {
        return 2 * x + 40 * Math.cos(x);
    }
    // f''(x) = 2
    static double fpp ( double x ) {
        return 2 + 40 * -1 * Math.sin(x);
    }

    public static void main1d( String[] args ) {
        double tolerance = 0.0001;
        double step_size = 0.1;

        double x_0 = 50.0;
        double x_n = x_0;
        int count = 0;
        while ( Math.abs(fp(x_n)) > tolerance && count < 500 ) {
            count++;
            // Gradient Descent:
            double vp = fp(x_n);
            System.out.println("x_n = " + x_n + " fp(x_n) = " + vp);
            x_n = x_n - step_size * vp;
            // Newton's
            /*double vp = fp(x_n);
              double vpp = fpp(x_n);
              System.out.println("x_n = " + x_n + " fp(x_n) = " + vp + " fpp(x_n) = " + vpp);
              x_n = x_n - vp / vpp;*/
            System.out.println(count + ": Going to... " + x_n);
        }
        System.out.println("The best value is " + x_n);


    }

    static double err_for_line(int N, double Xs[], double Ys[],
                               double m, double b) {
        double total_error = 0.0;
        for ( int i = 0; i < N; i++ ) {
            double this_error =
                Math.pow(Ys[i] - (m * Xs[i] + b), 2);
            total_error = total_error + this_error;
        }
        return total_error / N;
    }

    public static void main( String[] args ) {
        Random r = new Random();
        double M = 10 * r.nextDouble();
        double B = 25 * r.nextDouble();
        System.out.println("M = " + M + " B = " + B);

        int N = 1000;

        double Xs[] = new double[N];
        double Ys[] = new double[N];
        System.out.print("(");
        for ( int i = 0; i < N; i++ ) {
            Xs[i] = 0.25 * N * r.nextDouble();
            double err = 0.5 - r.nextDouble();
            Ys[i] = (M * Xs[i] + B) + (10 * err);
            System.out.println("(" + Xs[i] + " " + Ys[i] + ")");
        }
        System.out.println(")");

        double m_n = 1.0;
        double b_n = 0.0;
        int count = 0;

        double step_size = 0.0000005 ;
        
        while ( count < 100 ) {
            count++;

            System.out.println(count + ". (" + m_n + "," + b_n + ") = " + err_for_line(N, Xs, Ys, m_n, b_n));

            double b_gradient = 0.0;
            double m_gradient = 0.0;
            double Nd = N;

            for ( int i = 0; i < N; i++ ) {
                double err =
                    (Ys[i] - ((m_n*Xs[i]) + b_n));
                b_gradient -= err;
                m_gradient -= Xs[i] * err;
            }
            b_gradient *= 2.0;
            m_gradient *= 2.0;
            b_gradient /= Nd;
            m_gradient /= Nd;

            m_n = m_n - step_size * m_gradient;
            b_n = b_n - step_size * b_gradient;
        }

    }
}
