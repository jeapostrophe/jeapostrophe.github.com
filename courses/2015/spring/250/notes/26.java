import java.util.Random;

/* Example Thread */
class HelloRunnable implements Runnable {
    public void run() {
        System.out.println("Hello from a thread!");
    }

    public static void main(String args[]) {
        (new Thread(new HelloRunnable())).start();
    }

    }

// t.join() to wait

class ParaSort implements Runnable {
    ParaSorter ps; int start; int end; int level;
    ParaSort(ParaSorter _ps, int _start, int _end, int _level) {
        ps = _ps;
        start = _start;
        end = _end;
        level = _level;
    }
    public void run() {
        try { ps.para_sort(start, end, level); }
        catch (java.lang.InterruptedException x) {
        }
    }
}

class ParaSorter {
    static int N = 3200000;
    int vals[];

    ParaSorter() {
        this.vals = new int[N];
    }

    void swap(int from, int to) {
        int tmp = vals[from];
        //System.out.println("swap " + from + " " + to);
        vals[from] = vals[to];
        vals[to] = tmp;
    }

    void serial_merge(int begin1, int end1, int begin2, int end2) {
        //System.out.println("sm " + begin1 + " " + end1 + " " + begin2 + " " + end2);
        //System.out.print("before "); print_all();
        while ( begin1 < end1 ) {
            if ( vals[begin1] < vals[begin2] ) {
                begin1++;
            } else {
                swap(begin1, begin2);
                begin1++;
                begin2++;                
            }
        }
        //System.out.print("after "); print_all();

    }

    void serial_sort(int start, int end) {
        // System.out.println("ss " + start + " " + end);
        // print_all();
        int size = end - start;
        if ( size == 1 ) {
            return;
        } else {
            int halfsize = size / 2;
            int halfway = start + halfsize;
            serial_sort(start, halfway);
            serial_sort(halfway, end);
            serial_merge(start, start + halfsize,
                         halfway, halfway + halfsize);
        }
    }

    void para_sort(int start, int end, int level) throws java.lang.InterruptedException {
        int size = end - start;
        if ( size == 1 ) {
            return;
        } else {
            int halfsize = size / 2;
            int halfway = start + halfsize;
            // Fine Grained -- Means many threads that do small work
            // Coarse grained -- Means Few threads that do a lot of work
            if ( level < 2 ) {
                ParaSort ps1 = new ParaSort(this, start, halfway, level+1);
                ParaSort ps2 = new ParaSort(this, halfway, end, level+1);
                Thread t1 = (new Thread(ps1));
                Thread t2 = (new Thread(ps2));
                t1.start();
                t2.start();
                t1.join();
                t2.join();
            } else {
                para_sort(start, halfway, level+1);
                para_sort(halfway, end, level+1);
            }
            serial_merge(start, start + halfsize,
                         halfway, halfway + halfsize);
        }        
    }

    void print_all() {
        for (int i = 0; i < N; i++) {
            System.out.print(vals[i] + " ");
        }
        System.out.println("");
    }

    void initialize() {
        Random r = new Random();
        for (int i = 0; i < N; i++) {
            vals[i] = r.nextInt(N);
        }
    }
}

class C26b {
    public static void main(String[] args) {
        ParaSorter ps = new ParaSorter();
        
        System.out.println("Here");

        for ( int i = 0; i < 2; i++ ) {
            ps.initialize();
            long start = System.currentTimeMillis();
            switch (i) {
            case 0:
                ps.serial_sort(0, ParaSorter.N);
                break;
            case 1:
                try {
                    ps.para_sort(0, ParaSorter.N, 0);
                } catch (java.lang.InterruptedException x) {
                    break;
                };
                break;
            }
            long end = System.currentTimeMillis();
            System.out.println("time: " + (end - start));
        }
        
    }
}
