package de.tuda.stg.consys.demo.crdts;

import de.tuda.stg.consys.demo.JBenchExecution;


public class StartBenchmark {

    public static void main(String[] args) {
        JBenchExecution.execute("crdts", CRDTBenchmarkRunnable.class, args);
    }
}
