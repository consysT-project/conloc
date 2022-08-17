package de.tuda.stg.consys.demo;

import de.tuda.stg.consys.bench.BaseBenchmarkConfig;
import de.tuda.stg.consys.bench.BenchmarkStoreFactory;
import de.tuda.stg.consys.bench.BenchmarkConfig;
import de.tuda.stg.consys.core.store.akka.AkkaStore;
import de.tuda.stg.consys.utils.Logger;
import org.apache.commons.cli.*;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.Executors;
import java.util.concurrent.Future;
import java.util.concurrent.TimeUnit;


public class JBenchExecution {

    public static void execute(String name, Class<? extends JBenchRunnable> clazz, String[] args) {

        Options options = new Options();

        Option backendOption = Option.builder()
                .argName("store backend")
                .option("b")
                .longOpt("backend")
                .hasArg()
                .desc("The backend to execute this benchmark.")
                .required()
                .build();
        options.addOption(backendOption);

        Option configOption = Option.builder()
                .argName("configuration files")
                .option("c")
                .longOpt("config")
                .hasArgs()
                .desc("The name of the configuration files. If multiple files are given then spawns a new thread for a store on each file. Otherwise, use this thread to execute the single file.")
                .required()
                .build();
        options.addOption(configOption);

        CommandLineParser parser = new DefaultParser();
        HelpFormatter formatter = new HelpFormatter();
        CommandLine cmd = null;

        try {
            cmd = parser.parse(options, args);
        } catch (ParseException e) {
            Logger.err(e.getMessage());
            formatter.printHelp("consys-benchmark " + name, options);
            System.exit(1);
        }


        String[] configNames = cmd.getOptionValues("config");

        if (configNames.length == 1) {
            BenchmarkConfig config = BaseBenchmarkConfig.load(name, configNames[0], false);
            JBenchExecutor executor = createExecutor(cmd, name, clazz, config);
            executor.runBenchmark();
        } else if (configNames.length > 1) {
            runMultiThread(cmd, name, clazz, configNames);
        } else {
            throw new IllegalArgumentException("not enough configs given: " + Arrays.toString(configNames));
        }
    }

    private static void runMultiThread(CommandLine cmd, String name, Class<? extends JBenchRunnable> clazz, String[] configNames) {
        int numberOfThreads = configNames.length;

        var exec = Executors.newFixedThreadPool(numberOfThreads);
        List<Future<?>> futures = new ArrayList<>();

        for (int i = 0; i < numberOfThreads; i++) {
            final String configName = configNames[i];
            futures.add(exec.submit(() -> {
                try {
                    BenchmarkConfig config = BaseBenchmarkConfig.load(name, configName, true);
                    JBenchExecutor executor = createExecutor(cmd, name, clazz, config);
                    executor.runBenchmark();
                } catch (Exception e) {
                    e.printStackTrace();
                }
            }));
        }

        try {
            exec.shutdown();
            exec.awaitTermination(60, TimeUnit.SECONDS);
            for (var future : futures) {
                future.get(); // gets exceptions not caught inside thread
            }
        } catch (InterruptedException | ExecutionException e) {
            throw new RuntimeException(e);
        }
    }



    private static JBenchExecutor createExecutor(CommandLine cmd, String name, Class<? extends JBenchRunnable> clazz, BenchmarkConfig config) {
        String backend = cmd.getOptionValue("backend");

        if ("akka".equals(backend)) {
            return akkaExecutor(name, clazz, config);
        } else {
            throw new IllegalArgumentException("unknown backend: " + backend);
        }
    }

    private static JBenchExecutor akkaExecutor(String name, Class<? extends JBenchRunnable> clazz, BenchmarkConfig config) {
        BenchmarkStoreFactory<AkkaStore> factory = BenchmarkStoreFactory.akkaStoreFactory();
        AkkaStore scalaStore = factory.apply(config.toConfig());
        JBenchStore store = JBenchStore.fromAkkaStore(scalaStore);

        return new JBenchExecutor(
                name,
                config,
                store,
                JBenchOperationFactory.fromClass(clazz).create(store, config)
        );
    }

}
