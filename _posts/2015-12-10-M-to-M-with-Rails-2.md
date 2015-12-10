---
layout: post
title: Many to Many Relationships with Rails, part 2
---

### `has_many :through`
In the previous post we created a M:M relationship between **Users** and **Group** using Rails's `has_and_belongs_to_many` method. While still part of the standard, this way of doing it has fallen out of favor lately and instead the method `has_many :through` is usually used instead.

The main difference between the two is that `has_and_belongs_to_many` implicitly creates a join table and model linking the two models, while with `has_many :through` requires you to explicitly create the join model and the table in the database.

Using the same example, we'll create a M:M relationship between **User** and **Group**.

The models before adding the relationship:

*models/user.rb*
{% highlight ruby %}
class User < ActiveRecord::Base
end
{% endhighlight %}

*models/group.rb*
{% highlight ruby %}
class Group < ActiveRecord::Base
end
{% endhighlight %}

*models/user_group.rb*
{% highlight ruby %}
class UserGroup < ActiveRecord::Base
end
{% endhighlight %}

As you can see, unlike with `has_and_belongs_to_many`, we now have to add the **UserGroup** model ourselves, this is where the foreign keys of both **User** and **Group** will reside, linking the two models.

Then we have to add a 1:M between **Group** and **UserGroup**, and also between **Group** and **UserGroup**.

{% highlight ruby %}
class User < ActiveRecord::Base
  has_many :user_groups
end

class Group < ActiveRecord::Base
  has_many :user_groups
end

class UserGroup < ActiveRecord::Base
  belongs_to :user
  belongs_to :group
end
{% endhighlight %}

Now we've hooked up both **User** and **Group** to **UserGroup**, it's time to link them with `:through` and create a M:M relationship.

{% highlight ruby %}
class User < ActiveRecord::Base
  has_many :user_groups
  has_many :groups, through: :user_groups
end

class Group < ActiveRecord::Base
  has_many :user_groups
  has_many :users, through: :user_groups
end
{% endhighlight %}

Let's test it out on the Ruby Console:

    2.0.0-p647 :010 > User
     => User(Table doesn't exist) 
    2.0.0-p647 :012 > Group
     => Group(Table doesn't exist) 
    2.0.0-p647 :014 > UserGroup
     => UserGroup(Table doesn't exist) 
    2.0.0-p647 :017 > 

So the models seem to load correctly, but there aren't any database tables that correspond to them.

### Migration
Let's do the migrations and setup the database. First we create the migration files.

    ~/dev/ruby/example ruby-2.0.0-p647 » rails generate migration create_users                                                                                                                                                                          mo@x1[79%]
          invoke  active_record
          create    db/migrate/20151210061204_create_users.rb
    ------------------------------------------------------------
    ~/dev/ruby/example ruby-2.0.0-p647 » rails generate migration create_groups                                                                                                                                                                         mo@x1[79%]
          invoke  active_record
          create    db/migrate/20151210061216_create_groups.rb
    ------------------------------------------------------------
    ~/dev/ruby/example ruby-2.0.0-p647 » rails generate migration create_user_groups                                                                                                                                                                    mo@x1[79%]
          invoke  active_record
          create    db/migrate/20151210061250_create_user_groups.rb

Then setup the database tables. Here we give each group a *name*, each user a *username*, and the join table will have a *user_id* and a *group_id* associated to it for each row.

*db/migrations/timestamp_create_groups.rb*
{% highlight ruby %}
class CreateGroups < ActiveRecord::Migration
  def change
    create_table :groups do |t|
      t.string :name
      t.timestamps
    end
  end
end
{% endhighlight %}

*db/migrations/timestmap_create_users.rb*
{% highlight ruby %}
class CreateUsers < ActiveRecord::Migration
  def change
    create_table :users do |t|
      t.string :username
    end
  end
end
{% endhighlight %}

*db/migrations/timestamp_create_user_groups*
{% highlight ruby %}
class CreateUserGroups < ActiveRecord::Migration
  def change
    create_table :user_groups do |t|
      t.integer :user_id
      t.integer :group_id
    end
  end
end
{% endhighlight %}

And finally run the migration.

    ~/dev/ruby/example ruby-2.0.0-p647 » rake db:migrate                                                                                                                                                                                                mo@x1[79%]
    == 20151210061204 CreateUsers: migrating ======================================
    -- create_table(:users)
       -> 0.0010s
    == 20151210061204 CreateUsers: migrated (0.0010s) =============================
    
    == 20151210061216 CreateGroups: migrating =====================================
    -- create_table(:groups)
       -> 0.0004s
    == 20151210061216 CreateGroups: migrated (0.0005s) ============================
    
    == 20151210061250 CreateUserGroups: migrating =================================
    -- create_table(:user_groups)
       -> 0.0005s
    == 20151210061250 CreateUserGroups: migrated (0.0005s) ========================

### Testing the Relationships
Now let's populate the database with users and groups.

    2.0.0-p647 :015 > mo = User.create(username: "Mo")
    2.0.0-p647 :023 > andrea = User.create(username: "Andrea")
    2.0.0-p647 :029 > mara = User.create(username: "Mara")
    2.0.0-p647 :039 > google = Group.create(name: "Google")
    2.0.0-p647 :047 > yahoo = Group.create(name: "Yahoo")
    2.0.0-p647 :064 > bing = Group.create(name: "Bing")
    2.0.0-p647 :071 > 

Assigning groups to users:

    2.0.0-p647 :035 > mo.groups << google
    2.0.0-p647 :045 > mo.groups << yahoo
    2.0.0-p647 :055 > andrea.groups << google
    2.0.0-p647 :077 > andrea.groups << bing
    2.0.0-p647 :083 > mara.groups << yahoo
    2.0.0-p647 :087 > 

Testing the relationships:

    2.0.0-p647 :124 > mo.groups
    => #<ActiveRecord::Associations::CollectionProxy [
    #<Group id: 1, name: "Google", created_at: "2015-12-10 06:30:50", updated_at: "2015-12-10 06:30:50">,
    #<Group id: 2, name: "Yahoo", created_at: "2015-12-10 06:31:01", updated_at: "2015-12-10 06:31:01">]>

    2.0.0-p647 :128 > andrea.groups
    => #<ActiveRecord::Associations::CollectionProxy [
    #<Group id: 1, name: "Google", created_at: "2015-12-10 06:30:50", updated_at: "2015-12-10 06:30:50">,
    #<Group id: 3, name: "Bing", created_at: "2015-12-10 06:31:08", updated_at: "2015-12-10 06:31:08">]>

    2.0.0-p647 :130 > mara.groups
    => #<ActiveRecord::Associations::CollectionProxy [
    #<Group id: 2, name: "Yahoo", created_at: "2015-12-10 06:31:01", updated_at: "2015-12-10 06:31:01">]>

So the that seems to work ok, now how about seeing if you can retrieve the users of each group.

    2.0.0-p647 :132 > google.users
    => #<ActiveRecord::Associations::CollectionProxy [
    #<User id: 1, username: "Mo", created_at: "2015-12-10 06:30:18", updated_at: "2015-12-10 06:30:18">,
    #<User id: 2, username: "Andrea", created_at: "2015-12-10 06:30:27", updated_at: "2015-12-10 06:30:27">]>

    2.0.0-p647 :140 > yahoo.users
    => #<ActiveRecord::Associations::CollectionProxy [
    #<User id: 1, username: "Mo", created_at: "2015-12-10 06:30:18", updated_at: "2015-12-10 06:30:18">,
    #<User id: 3, username: "Mara", created_at: "2015-12-10 06:30:34", updated_at: "2015-12-10 06:30:34">]>

    2.0.0-p647 :143 > bing.users
      User Load (0.1ms)  SELECT "users".* FROM "users" INNER JOIN "user_groups" ON "users"."id" = "user_groups"."user_id" WHERE "user_groups"."group_id" = ?  [["group_id", 3]]
      => #<ActiveRecord::Associations::CollectionProxy [
      #<User id: 2, username: "Andrea", created_at: "2015-12-10 06:30:27", updated_at: "2015-12-10 06:30:27">]>

Success! The relationships have been established.

### The Difference Between the Two
The Rails documentation on the [topic](http://guides.rubyonrails.org/association_basics.html#choosing-between-has-many-through-and-has-and-belongs-to-many) states that if you don't need to do anything with the join table (UserGroup) then it may be simpler to use `has_and_belongs_to_many` over `has_many :through`, however in practice the time you save on typing is minimal, and since you can't use validations, callbacks, etc. on the join model it makes more sense to default to using `has_many :through`.
