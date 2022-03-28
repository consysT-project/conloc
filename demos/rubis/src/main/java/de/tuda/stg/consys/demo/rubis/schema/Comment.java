package de.tuda.stg.consys.demo.rubis.schema;

import de.tuda.stg.consys.japi.Ref;

import java.util.Date;

public class Comment {
    int rating;
    String message;
    Ref<User> fromUser;
    Ref<User> toUser;
    Date date;

    public Comment(int rating, String message, Ref<User> fromUser, Ref<User> toUser) {
        this.rating = rating;
        this.message = message;
        this.fromUser = fromUser;
        this.toUser = toUser;
        this.date = new Date();
    }
}
