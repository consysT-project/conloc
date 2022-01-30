package de.tuda.stg.consys.demo.rubis;

import de.tuda.stg.consys.annotations.Transactional;
import de.tuda.stg.consys.japi.Ref;

import java.util.List;
import java.util.Optional;
import java.util.UUID;

public class Util {
    @Transactional
    public static void closeAuction(Ref<Item> item, Ref<AuctionStore> rubis) {
        Ref<User> seller = item.ref().getSeller();
        Optional<Ref<Bid>> winningBid = item.ref().closeAuction();

        if (winningBid.isPresent()) {
            Ref<User> winner = winningBid.get().ref().getUser();
            float price = winningBid.get().ref().getBid();

            winner.ref().removeBalance(price);
            seller.ref().addBalance(price);

            winner.ref().closeWatchedAuction(item);

            winner.ref().notifyWinner(item, price);
        }

        seller.ref().closeOwnAuction(item);

        UUID id = item.ref().getId();
        Category cat = item.ref().getCategory();
        rubis.ref().closeAuction(id, cat);
        //rubis.ref().closeAuction(item.ref().getId(), item.ref().getCategory()); // runtime error ref() not resolved by compiler
    }

    @Transactional
    public static void buyItemNow(Ref<Item> item, Ref<User> buyer, Ref<AuctionStore> rubis)
            throws AppException, NotEnoughCreditsException {
        float price = item.ref().buyNow();

        if (!hasEnoughCredits(buyer, price)) {
            throw new NotEnoughCreditsException();
        }

        Ref<User> seller = item.ref().getSeller();
        buyer.ref().removeBalance(price);
        seller.ref().addBalance(price);

        buyer.ref().closeWatchedAuction(item);
        seller.ref().closeOwnAuction(item);

        buyer.ref().notifyWinner(item, price);

        UUID id = item.ref().getId();
        Category cat = item.ref().getCategory();
        rubis.ref().closeAuction(id, cat);
    }

    @Transactional
    public static boolean hasEnoughCredits(Ref<User> buyer, float price) {
        List<Ref<Item>> watched = buyer.ref().getOpenBuyerAuctions();
        float potentialBalance = buyer.ref().getBalance();

        for (var item : watched) {
            Optional<Ref<Bid>> bid = item.ref().getTopBid();
            if (bid.isPresent() && ((Ref<User>)bid.get().ref().getUser()).ref().getNickname().equals(buyer.ref().getNickname())) {
                potentialBalance -= (float)bid.get().ref().getBid();
            }
        }

        return potentialBalance >= price;
    }
}
