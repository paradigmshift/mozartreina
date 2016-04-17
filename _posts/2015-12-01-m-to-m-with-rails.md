---
layout: post
title: Many to Many Relationships with Rails, part 1
---

### 2 Different Ways
In Rails there are two main ways to setup a many-to-many relationship between your models: [has_many :through](http://guides.rubyonrails.org/association_basics.html#the-has-many-through-association), which links one model to another via an intermediary model, and [has_and_belongs_to_many](http://guides.rubyonrails.org/association_basics.html#the-has-and-belongs-to-many-association), which creates a direct association between two models with no intervening third model.

Here we will discuss how to setup a **has_and_belongs_to_many** association, since I feel that there are a few stumbling blocks regarding this and while there *are* resources around that talk about how to do it properly, they're a bit spread-out and not very easy to find.

### The Basics
Let's say that you have two models in your application and that suddenly you would like to create a direct, many-to-many (hereonafter we will use the designation M:M) relationship between them. For example, your have different users and different groups and would like to link them, a user may belong to many groups and a group may have many users.

Since the models have already been created, we just modify them to contain the **has_and_belongs_to_many** keyword.

*models/user.rb*
{% highlight ruby %}
class User < ActiveRecord::Base
  has_and_belongs_to_many :groups
end
{% endhighlight %}

*models/group.rb*
{% highlight ruby %}
class Group < ActiveRecord::Base
  has_and_belongs_to_many :users
end
{% endhighlight %}

Then we have to create a migration. Note that with **has_and_belongs_to_many**, a linking or join table (a table that links the two models together) is needed, and by default the name is the combination, alphabetically, of the two models. So in our case, it would be *group_users*. So we have to use this convention when creating the migration, otherwise the models will not know which database table is linking them.

    rails generate migration groups_users

Then we open up the file that it created and make sure it looks like this:

{% highlight ruby %}
class GroupsUsers < ActiveRecord::Migration
  def change
    create_table :groups_users, :id => false do |t|
      t.integer :group_id
      t.integer :user_id
    end
  end
end
{% endhighlight %}

The last thing we do before we test it all out in the Rails Console is setup the acceptable parameters when we create new users.

{% highlight ruby %}
def create
  @user = User.new(user_params)
....
....
def user_params
  params.require(:user).permit(:name, :group_ids => [])
end
{% endhighlight %}

**:group_ids** is a method which is created when you specify that `User` `has_and_belongs_to_many` `groups`. The complete list of auto-generated methods can be found [here](http://api.rubyonrails.org/classes/ActiveRecord/Associations/ClassMethods.html#method-i-has_and_belongs_to_many), the ones we will use will be `User#group_ids` and `User#groups<<`.

Now in the Rails Console we can create our users and groups and link them.

    google = Group.create!(:name => "Google")
    aol = Group.create!(:name => "AOL")
    user1 = User.create!(:name => "Joe")
    user2 = User.create!(:name => "Jim")
    user3 = User.create!(:name => "George")
    user4 = User.create!(:name => "John")
    user1.groups << google
    user1.groups << aol
    user2.groups << aol
    user3.groups << google

Here we've created 2 groups and 4 users, `user1` belongs to both groups, `user2` to one, `user3` to the other one, and `user4` to neither. Now to test the associations. Let's try grabbing only the users that have groups.

    => #<ActiveRecord::Relation [#<User id: 5, name: "Joe", email: nil, created_at: "2015-12-01 10:41:08", updated_at: "2015-12-01 10:41:08">,
    #<User id: 6, name: "Jim", email: nil, created_at: "2015-12-01 10:41:18", updated_at: "2015-12-01 10:41:18">,
    #<User id: 7, name: "George", email: nil, created_at: "2015-12-01 10:41:27", updated_at: "2015-12-01 10:41:27">]>

So as we can see, only "Joe", "Jim", and "George" are returned. How about finding all the users that are members of the group *Google*?

    User.joins(:groups).where(groups: {name:"Google"})
    User Load (0.2ms)  SELECT "users".* FROM "users" INNER JOIN "groups_users" ON "groups_users"."user_id" = "users"."id" INNER JOIN "groups" ON "groups"."id" = "groups_users"."group_id" WHERE "groups"."name" = ?  [["name", "Google"]]
    => #<ActiveRecord::Relation [#<User id: 5, name: "Joe", email: nil, created_at: "2015-12-01 10:41:08", updated_at: "2015-12-01 10:41:08">,
    #<User id: 7, name: "George", email: nil, created_at: "2015-12-01 10:41:27", updated_at: "2015-12-01 10:41:27">]>

Members of *AOL*?

    User.joins(:groups).where(groups: {name:"AOL"})
    User Load (0.2ms)  SELECT "users".* FROM "users" INNER JOIN "groups_users" ON "groups_users"."user_id" = "users"."id" INNER JOIN "groups" ON "groups"."id" = "groups_users"."group_id" WHERE "groups"."name" = ?  [["name", "AOL"]]
    => #<ActiveRecord::Relation [#<User id: 5, name: "Joe", email: nil, created_at: "2015-12-01 10:41:08", updated_at: "2015-12-01 10:41:08">,
    #<User id: 6, name: "Jim", email: nil, created_at: "2015-12-01 10:41:18", updated_at: "2015-12-01 10:41:18">]>

Or all the groups that "Joe" is a member of?

    Group.joins(:users).where(users: {name:"Joe"})
    Group Load (0.5ms)  SELECT "groups".* FROM "groups" INNER JOIN "groups_users" ON "groups_users"."group_id" = "groups"."id" INNER JOIN "users" ON "users"."id" = "groups_users"."user_id" WHERE "users"."name" = ?  [["name", "Joe"]]
    => #<ActiveRecord::Relation [#<Group id: 1, name: "Google", created_at: "2015-11-30 14:37:15", updated_at: "2015-11-30 14:37:15">,
    #<Group id: 2, name: "AOL", created_at: "2015-11-30 14:46:57", updated_at: "2015-11-30 14:46:57">]>

### Setting up the views
So now we know that the associations work, but how do we use the views to add groups to users, or users to groups? Let's say you'd like to add the user to group as soon as a new user is created.

*controllers/users_controller.rb*
{% highlight ruby %}
def new
  @user = User.new
  @groups = Group.all
end
{% endhighlight %}

*views/users/new.html.erb*

    <div class="field">
      <%= f.label :name %><br>
      <%= f.text_field :name %>
    </div>
    <div class="field">
      <%= f.label :email %><br>
      <%= f.text_field :email %>
    </div>
    <div class="field">
      <%= collection_check_boxes(:user, :group_ids, @groups, :id, :name) %>
      </div>
    <div class="actions">
      <%= f.submit %>

On the third `div`, you'll notice that we have a [helper method](http://apidock.com/rails/v4.0.2/ActionView/Helpers/FormOptionsHelper/collection_check_boxes) from Rails to create checkboxes. For each member of `@groups` it will create a checkbox and assign a value and name based on the **:id** and **:name** of the member. The output looks like this:

{% highlight html %}
<div class="field">
  <input type="checkbox" value="1" name="user[group_ids][]" id="user_group_ids_1" />
    <label for="user_group_ids_1">Google</label>
  <input type="checkbox" value="2" name="user[group_ids][]" id="user_group_ids_2" />
    <label for="user_group_ids_2">AOL</label>
  <input type="hidden" name="user[group_ids][]" value="" />
</div>
{% endhighlight %}

where `value` is the id of the group and the text between the `<label>` tags is the name of the group.

### has_many :through
Next up we'll see how to create a M:M relationship with `has_many :through`, which is considered the default way of setting up M:M relationships in Rails these days.
