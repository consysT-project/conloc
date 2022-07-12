package de.tuda.stg.consys.demo.rubis;

import com.typesafe.config.Config;
import de.tuda.stg.consys.annotations.Transactional;
import de.tuda.stg.consys.bench.BenchmarkUtils;
import de.tuda.stg.consys.bench.OutputFileResolver;
import de.tuda.stg.consys.demo.CassandraDemoBenchmark;
import de.tuda.stg.consys.demo.rubis.schema.*;
import de.tuda.stg.consys.demo.rubis.schema.opcentric.Item;
import de.tuda.stg.consys.demo.rubis.schema.opcentric.User;
import de.tuda.stg.consys.japi.Ref;
import scala.Option;

import java.util.*;

@SuppressWarnings({"consistency"})
public class RubisBenchmark extends CassandraDemoBenchmark {
    public static void main(String[] args) {
        start(RubisBenchmark.class, args);
    }

    private final int numOfUsersPerReplica;
    private final float percentOfAuctionItems;
    private final List<Session> localSessions;
    private final List<UUID> localItems;
    private final List<Ref<? extends IItem>> allAuctionItems;
    private final List<Ref<? extends IItem>> allDirectBuyItems;
    private final List<Ref<? extends IUser>> users;
    private Ref<AuctionStore> auctionStore;

    private static final float maxPrice = 100;

    private static final List<String> WORDS = new ArrayList<>(Arrays.asList("small batch", "Etsy", "axe", "plaid", "McSweeney's", "VHS",
            "viral", "cliche", "post-ironic", "health", "goth", "literally", "Austin",
            "brunch", "authentic", "hella", "street art", "Tumblr", "Blue Bottle", "readymade",
            "occupy", "irony", "slow-carb", "heirloom", "YOLO", "tofu", "ethical", "tattooed",
            "vinyl", "artisan", "kale", "selfie"));
    private static final List<String> FIRST_NAMES = new ArrayList<>(Arrays.asList("Arthur", "Ford", "Tricia", "Zaphod"));
    private static final List<String> LAST_NAMES = new ArrayList<>(Arrays.asList("Dent", "Prefect", "McMillan", "Beeblebrox"));

    private final Random random = new Random();

    public RubisBenchmark(Config config, Option<OutputFileResolver> outputResolver) {
        super(config, outputResolver);

        numOfUsersPerReplica = config.getInt("consys.bench.demo.rubis.users");
        percentOfAuctionItems = 0.5f;

        Session.userConsistencyLevel = getStrongLevel();
        Session.itemConsistencyLevel = getStrongLevel();
        Session.storeConsistencyLevel = getStrongLevel();

        localSessions = new ArrayList<>();
        allAuctionItems = new ArrayList<>();
        allDirectBuyItems = new ArrayList<>();
        localItems = new ArrayList<>();
        users = new ArrayList<>();
    }

    private static String addr(String identifier, int grpIndex, int replIndex) {
        return identifier + "$" + grpIndex + "$"+ replIndex;
    }

    private String generateRandomName() {
        return FIRST_NAMES.get(random.nextInt(FIRST_NAMES.size()))
                + " " + LAST_NAMES.get(random.nextInt(LAST_NAMES.size()));
    }

    private String generateRandomPassword() {
        return WORDS.get(random.nextInt(WORDS.size()));
    }

    private String generateRandomText(int n) {
        String body = WORDS.get(random.nextInt(WORDS.size()));
        for (int i = 0; i < n - 1; i++)
            body += " " + WORDS.get(random.nextInt(WORDS.size()));
        return body;
    }

    private Category getRandomCategory() {
        return Category.values()[random.nextInt(Category.values().length)];
    }

    private float getRandomPrice(float max) {
        return random.nextFloat() * max;
    }

    private <E> E getRandomElement(List<E> list) {
        return list.get(random.nextInt(list.size()));
    }

    private <E> E getRandomElementExcept(List<E> list, E object) {
        E element;
        do {
            element = list.get(random.nextInt(list.size()));
        } while (element == object);
        return element;
    }

    protected float getInitialBalance() {
        return numOfUsersPerReplica * nReplicas() * maxPrice * 1.3f;
    }

    @Override
    public String getName() {
        return "RubisBenchmark";
    }

    @Override
    public void setup() {
        super.setup();

        Session.dataCentric = getBenchType() == BenchmarkType.MIXED;

        for (int i = 0; i < numOfUsersPerReplica; i++) {
            localSessions.add(new Session(store()));

            if (getBenchType() == BenchmarkType.MIXED) {
                localSessions.get(i).userImpl = de.tuda.stg.consys.demo.rubis.schema.datacentric.User.class;
                localSessions.get(i).itemImpl = de.tuda.stg.consys.demo.rubis.schema.datacentric.Item.class;
            } else {
                localSessions.get(i).userImpl = de.tuda.stg.consys.demo.rubis.schema.opcentric.User.class;
                localSessions.get(i).itemImpl = de.tuda.stg.consys.demo.rubis.schema.opcentric.Item.class;
            }
        }

        if (processId() == 0) {
            store().transaction(ctx -> {
                auctionStore = ctx.replicate(Util.auctionStoreKey, getStrongLevel(), AuctionStore.class);
                return Option.empty();
            });
        }

        barrier("auction_store_setup");

        if (processId() != 0) {
            store().transaction(ctx -> {
                auctionStore = ctx.lookup(Util.auctionStoreKey, getStrongLevel(), AuctionStore.class);
                return Option.empty();
            });
        }

        System.out.println("Adding users and items");
        for (int grpIndex = 0; grpIndex < numOfUsersPerReplica; grpIndex++) {

            localSessions.get(grpIndex).registerUser(null, addr("user", grpIndex, processId()), generateRandomName(),
                    generateRandomPassword(), "mail@example.com");

            localSessions.get(grpIndex).addBalance(null, getInitialBalance());

            localItems.add(localSessions.get(grpIndex).registerItem(null, generateRandomText(1), generateRandomText(10),
                    getRandomCategory(), getRandomPrice(maxPrice * 1.3f), 300));

            BenchmarkUtils.printProgress(grpIndex);
        }

        barrier("users_added");

        for (int grpIndex = 0; grpIndex < numOfUsersPerReplica; grpIndex++) {
            for (int replIndex = 0; replIndex < nReplicas(); replIndex++) {
                int localGrpIndex = grpIndex;
                int localReplIndex = replIndex;
                users.add(localSessions.get(0).findUser(null, addr("user", localGrpIndex, localReplIndex)));
                for (var cat : Category.values()) {
                    allAuctionItems.addAll(getRandomElement(localSessions).browseCategoryItems(null, cat));
                }
            }
        }
        for (int i = 0; i < percentOfAuctionItems * numOfUsersPerReplica; i++) {
            Ref<? extends IItem> item = allAuctionItems.remove(i);
            allDirectBuyItems.add(item);
        }
        BenchmarkUtils.printDone();
    }

    @Override
    public void cleanup() {
        super.cleanup();
        localSessions.clear();
        localItems.clear();
        allAuctionItems.clear();
        allDirectBuyItems.clear();
        users.clear();

        try {
            Thread.sleep(1000);
        } catch (InterruptedException e) {
            e.printStackTrace();
        }
    }

    @Override
    public void operation() {
        try {
            randomTransaction();
        } catch (AppException e) {
            /* possible/acceptable errors:
                - bidding on own item (rare)
                - auction has already ended (common)
            */
            System.out.println(e.getMessage());
        }
    }

    @Transactional
    private void randomTransaction() {
        int rand = random.nextInt(100);
        if (rand < 44) {
            // 44%
            browseCategory();
        } else if (rand < 66) {
            // 22%
            placeBid();
        } else if (rand < 81) {
            // 15%
            buyNow();
        } else if (rand < 92){
            // 11%
            rateUser();
        } else {
            // 9%
            closeAuction();
        }
    }

    private void placeBid() {
        //Ref<Item> item = getRandomElement(allAuctionItems);
        //Session session = getRandomElement(localSessions);

        store().transaction(ctx -> {
            List<Ref<? extends IItem>> openAuctions = auctionStore.ref().getOpenAuctions(); // TODO: is this ok? Overhead?
            if (openAuctions.isEmpty()) {
                System.out.println("no open auctions for placeBid operation");
                return Option.empty();
            }

            Ref<? extends IItem> item = getRandomElement(openAuctions);
            Session session = getRandomElement(localSessions);
            float bid = session.getBidPrice(ctx, item);
            session.placeBid(ctx, item, bid * (1 + random.nextFloat()));
            return Option.empty();
        });
    }

    private void buyNow() {
        //Ref<Item> item = getRandomElement(allDirectBuyItems);
        Session session = getRandomElement(localSessions);

        store().transaction(ctx -> {
            List<Ref<? extends IItem>> openAuctions = auctionStore.ref().getOpenAuctions(); // TODO: is this ok? Overhead?
            if (openAuctions.isEmpty()) {
                System.out.println("no open auctions for buyNow operation");
                return Option.empty();
            }

            var item = getRandomElement(openAuctions);
            session.buyNow(ctx, item);
            return Option.empty();
        });
    }

    private void closeAuction() {
        store().transaction(ctx -> {
            List<Ref<? extends IItem>> openAuctions = auctionStore.ref().getOpenAuctions(); // TODO: is this ok? Overhead?
            if (openAuctions.isEmpty()) {
                System.out.println("no open auctions for closeAuction operation");
                return Option.empty();
            }

            Ref<? extends IItem> item = getRandomElement(openAuctions);
            item.ref().endAuctionNow();
            item.ref().closeAuction(item);
            return Option.empty();
        });
    }

    private void browseCategory() {
        Category category = getRandomCategory();
        Session session = getRandomElement(localSessions);
        session.browseCategory(null, category, 5);
    }

    private void rateUser() {
        int rating = 1 + random.nextInt(5);
        Ref<? extends IUser> user1 = getRandomElement(users);
        Ref<? extends IUser> user2 = getRandomElementExcept(users, user1);
        store().transaction(ctx -> {
            user1.ref().rate(new Comment(rating, generateRandomText(10), user2, user1));
            return Option.empty();
        });
    }

    /**
     * Checked invariants:
     *  - ´user.balance´ is non-negative
     *  - ´user.balance´ corresponds to bought and sold items
     *  - auction winners corresponds to bought items
     *  - auction sellers correspond to sold items
     *  - winner is the highest bidder
     */
    @Override
    public void test() {
        if (processId() != 0) return;
        System.out.println("## TEST ##");

        check("users non empty", !users.isEmpty());

        for (var user : users) {
            store().transaction(ctx -> {
                float userBalance = user.ref().getBalance();
                check("balance >= 0", userBalance >= 0);

                float balance = getInitialBalance();
                for (var boughtItem : user.ref().getBuyerHistory()) {
                    if (boughtItem.ref().getSoldViaBuyNow()) {
                        balance -= boughtItem.ref().getBuyNowPrice();
                    } else {
                        var winningBidOption = boughtItem.ref().getTopBid();
                        check("buyer bid non null", winningBidOption.isPresent());
                        if (winningBidOption.isEmpty()) continue;

                        var winningBid = winningBidOption.get();
                        checkEquals("bid correct buyer", user.ref().getNickname(), winningBid.getUser().ref().getNickname());

                        var allBids = new ArrayList<>(boughtItem.ref().getAllBids());
                        allBids.remove(winningBid);
                        for (var bid : allBids) {
                            if (bid.getBid() >= winningBid.getBid())
                                check("winner bid is highest bid", false);
                        }

                        balance -= winningBid.getBid();
                    }
                }

                for (var soldItem : user.ref().getSellerHistory(true)) {
                    checkEquals("bid correct seller", user.ref().getNickname(), soldItem.ref().getSeller().ref().getNickname());

                    if (soldItem.ref().getSoldViaBuyNow()) {
                        balance += soldItem.ref().getBuyNowPrice();
                    } else {
                        var winningBidOption = soldItem.ref().getTopBid();
                        check("seller bid non null", winningBidOption.isPresent());
                        if (winningBidOption.isEmpty()) continue;

                        balance += winningBidOption.get().getBid();
                    }
                }

                checkFloatEquals("balance correct", balance, userBalance);

                return Option.empty();
            });
        }

        System.out.println("## TEST SUCCESS ##");
    }
}
