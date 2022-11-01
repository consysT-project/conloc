package de.tuda.stg.consys.demo.webshop.extras;


import de.tuda.stg.consys.japi.binding.cassandra.CassandraReplica;
import de.tuda.stg.consys.japi.binding.cassandra.CassandraStoreBinding;
import scala.concurrent.duration.Duration;

import java.util.Scanner;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;

@SuppressWarnings({"consistency"})
public class InteractiveSession {
    private static final CassandraStoreBinding[] replicas = new CassandraStoreBinding[3];
/*
    private static Session session;
*/
    private static ExecutorService threadPool;

    public static void main(String[] args) {
/*
        threadPool = Executors.newFixedThreadPool(backgroundTasks.length);
*/

        Scanner commandLine = new Scanner(System.in);
        System.out.println("auction client started\ntype 'connect' or 'init'");

        boolean running = true;
        String input;
        initConnections();
        while(running){
            System.out.print("> ");
            input = commandLine.nextLine();
            try {
                switch(input) {
                    case "buy": {
                        System.out.print("item id: ");
                        var id = commandLine.nextLine();
                        System.out.print("amount: ");
                        var amount = commandLine.nextLine();
/*
                        session.buy(id, amount);
*/
                        break;
                    }
                    case "exit":
                        running = false;
                        break;
                    default:
                        System.out.println("unknown command");
                        break;
                }
            } catch (Exception e) {
                System.out.println(e.getMessage());
                e.printStackTrace();
            }
        }
        commandLine.close();
        System.out.println("User stopped");
        closeConnections();
        System.out.println("Servers stopped");
    }

    private static void initConnections() {
        int zookeeperPort = 2181;
        for (int i = 0; i < replicas.length; i++)
            replicas[i] = CassandraReplica.create("127.0.0." + (i+1), 9042, zookeeperPort + i, Duration.apply(15, "s"), i == 0);

/*        for (int i = 0; i < backgroundTasks.length; i++) {
            backgroundTasks[i] = new BackgroundTask(i, replicas.length, msServerSleep, replicas[i % replicas.length]);
            backgroundTasks[i].init();
            threadPool.submit(backgroundTasks[i]);
        }

        session = new Session(replicas[0]);*/
    }

    private static void closeConnections() {
/*        for (var task : backgroundTasks)
            task.stopThread();

        threadPool.shutdown();
        //threadPool.awaitTermination(5, TimeUnit.SECONDS);

        try {
            for (var replica : replicas)
                replica.close();
        }
        catch (Exception e) {
            System.out.println(e.getMessage());
        }*/
    }
}
