---
title: "Sum Types In SQL"
date: 2019-03-19
layout: post
categories: programming
---

Algebraic datatypes are a powerful feature of functional programming languages.
By combining the expressive power of "and" and "or," we can solve all kinds of problems cleanly and elegantly.
SQL databases represent product types -- "and" -- extremely well - a SQL table can correspond easily and directly to a product type where each field in the product type can fit in a single column.

On the other hand, SQL databases have trouble with sum types -- "or".
Most SQL databases support simple enumerations easily, but they lack the ability to talk about real sum types with fields.
We can encode sum types in SQL in a few different ways, each of which has upsides and downsides.

For each of these examples, we will be encoding the following Haskell datatype:

```haskell
data Animal
    = Cat Name Age FavoriteFood
    | Dog Name OwnerId
    | Bird Name Song

type Name = Text
type Age = Int
type FavoriteFood = Text
type OwnerId = Int
type Song = Text
```

We're going to ignore that this datatype could be normalized (though I will describe datatype normalization and show what I mean at the end of the post).

The SQL examples in this blog post will use PostgreSQL.
The Haskell query code will use the `persistent` syntax for entities and queries.

# Shared Primary Keys

The first technique is the one that I have used with the most success.
It has a good set of tradeoffs with respect to SQL normalization and Haskell types.

First, we are going to create a table for all animals and a type for the constructors:

```sql
CREATE TYPE animal_constr AS ENUM ('cat', 'bird', 'dog');

CREATE TABLE animal (
  id    SERIAL          NOT NULL,
  type  animal_constr   NOT NULL,

  PRIMARY KEY (id, type)
);
```

The `type` field on the table will distinguish the different constructors and tables we'll use.

Let's create the `cat` table:

```sql
CREATE TABLE cat (
    id      
        INTEGER PRIMARY KEY,
    type 
        animal_constr NOT NULL 
        DEFAULT 'cat' 
        CHECK (type = 'cat'),
    name    
        TEXT    NOT NULL,
    age
        INTEGER NOT NULL,
    favorite_food 
        TEXT 	NOT NULL,
  
    FOREIGN KEY (id, type) 
        REFERENCES animal (id, type)
)
```

The contents of the `type` column for the `cat` table are completely constrained -- we cannot insert any record into the table that does not have a value of type `'cat'`.
Fortunately, this is automated - we can omit specifying it, and it'll be filled in automatically from the `DEFAULT 'cat'` clause.

The other SQL tables will have a similar format.
The `type` field will be constrained to the constructor.

If we add new constructors to the Haskell datatype, we can use the SQL command:

```sql
ALTER TYPE animal_constr ADD VALUE 'alligator';
```

Then we can create an additional table `'alligator'`, and the constraints all work out exactly like we want.

This representation has a downside.
It is possible to insert entries into the `animal` table which have no corresponding entry in any other table.
Indeed, this is necessary - there must first be an entry in the `animal` table before we can insert the corresponding `cat`.

Let's check out how to query this from Haskell.
We are going to use the following `persistent` definition:

```
Animal
    type         AnimalConstr

Cat
    type         AnimalConstr
    name         Text
    age          Int
    favoriteFood Text
```

This corresponds exactly with the SQL description above, though it will generate migrations that are different.
I am going to elide the `PersistField` definitions, as they are irrelevant, but `AnimalConstr` is defined as:

```haskell
data AnimalConstr = Cat | Dog | Bird
    deriving (Eq, Show, Read)
```

Assuming we have defined the other animal tables, we'll use this SQL query:

```sql
SELECT ??, ??, ??, ??
FROM animal
LEFT JOIN cat
    ON cat.id = animal.id
LEFT JOIN dog
    ON dog.id = animal.id
LEFT JOIN bird
    ON bird.id = animal.id
WHERE cat.id IS NOT NULL
    OR dog.id IS NOT NULL
    OR bird.id IS NOT NULL
```

We need the `WHERE` clause to ensure that we do not select `animal` records that do not have corresponding records in the subtables.

The Haskell code to load these records out of the database looks like this:

```haskell
selectAnimalsDb
    :: SqlPersistM
        [ ( Entity Animal
          , Maybe (Entity Cat)
          , Maybe (Entity Dog)
          , Maybe (Entity Bird)
          )
        ]
selectAnimalsDb = rawQuery theAboveSqlQuery []
```

We have a post-condition on the return value of this query that is guaranteed by the database schema, but not present in the types.
One of the `Maybe` values *will* be a `Just` constructor, and the `Just` constructor will be determined by the `AnimalConstr` value on the `Entity Animal` value.
This allows us to *unsafely* extract the `Just` value based on the constructor.

We would write our conversion function as so:

```haskell
toDomainType 
    :: Entity Animal
    -> Maybe (Entity Cat)
    -> Maybe (Entity Dog)
    -> Maybe (Entity Bird)
    -> Domain.Animal
toDomainType (Entity _ animal) mcat mdog mbird =
    case (animalType animal, mcat, mdog, mbird) of
        (Cat, Just cat, _, _) ->
            toCat cat
        (Dog, _, Just dog, _) ->
            toDog dog
        (Bird, _, _, Just bird) ->
            toBird bird
        _ ->
            error "Impossible due to database constraints"
  where
    toDog  :: Entity Dog  -> Animal
    toCat  :: Entity Cat  -> Animal
    toBird :: Entity Bird -> Animal
```

This approach is safe, easy to extend, and conforms to good relational database design.
It is possible to make the Haskell-side even safer, by adding some fancier type-level tricks to the SQL conversion layer.
However, that differs based on the database library you are using, and I would like to keep this post generalizable to other libraries.
A followup post (or library?) might provide insight into this.

# The `persistent` Approach

The `persistent` library has an approach to encoding sum-type entities.
The [documentation](https://github.com/yesodweb/persistent/blob/master/docs/Persistent-entity-syntax.md#entity-level) describes this feature.
Given that I wrote the documentation, I'm going to copy it in here:

The schema in the test is reproduced here:

```haskell
share [mkPersist persistSettings, mkMigrate "sumTypeMigrate"] [persistLowerCase|
Bicycle
    brand T.Text
Car
    make T.Text
    model T.Text
+Vehicle
    bicycle BicycleId
    car CarId
|]
```

Let's check out the definition of the Haskell type `Vehicle`.
Using `ghci`, we can query for `:info Vehicle`:

```
>>> :i Vehicle
type Vehicle = VehicleGeneric SqlBackend
        -- Defined at .../Projects/persistent/persistent-test/src/SumTypeTest.hs:26:1

>>> :i VehicleGeneric
type role VehicleGeneric nominal
data VehicleGeneric backend
  = VehicleBicycleSum (Key (BicycleGeneric backend))
  | VehicleCarSum (Key (CarGeneric backend))
        -- Defined at .../persistent/persistent-test/src/SumTypeTest.hs:26:1
-- lots of instances follow...
```

A `VehicleGeneric` has two constructors:

- `VehicleBicycleSum` with a `Key (BicycleGeneric backend)` field
- `VehicleCarSum` with a `Key (CarGeneric backend)` field

The `Bicycle` and `Car` are typical `persistent` entities.

This generates the following SQL migrations (formatted for readability):

```sql
CREATE TABLE "bicycle" (
    "id"        INTEGER PRIMARY KEY,
    "brand"     VARCHAR NOT NULL
);

CREATE TABLE "car"(
    "id"        INTEGER PRIMARY KEY,
    "make"      VARCHAR NOT NULL,
    "model"     VARCHAR NOT NULL
);

CREATE TABLE "vehicle"(
    "id"        INTEGER PRIMARY KEY,
    "bicycle"   INTEGER NULL REFERENCES "bicycle",
    "car"       INTEGER NULL REFERENCES "car"
);
```

The `vehicle` table contains a nullable foreign key reference to both the bicycle and the car tables.

A SQL query that grabs all the vehicles from the database looks like this (note the `??` is for the `persistent` raw SQL query functions):

```sql
SELECT ??, ??, ??
FROM vehicle
LEFT JOIN car
    ON vehicle.car = car.id
LEFT JOIN bicycle
    ON vehicle.bicycle = bicycle.id
```

If we use the above query with `rawSql`, we'd get the following result:

```haskell
getVehicles 
    :: SqlPersistM 
        [ ( Entity Vehicle
          , Maybe (Entity Bicycle)
          , Maybe (Entity Car)
          )
        ]
```

This result has some post-conditions that are not guaranteed by the types *or* the schema.
The constructor for `Entity Vehicle` is going to determine which of the other members of the tuple is `Nothing`.
We can convert this to a friendlier domain model like this:

```haskell
data Vehicle'
    = Car' Text Text
    | Bike Text

check = do
    result <- getVehicles
    pure (map convert result)

convert 
    :: Entity Vehicle
    -> Maybe (Entity Bicycle)
    -> Maybe (Entity Car)
    -> Vehicle'
convert (Entity _ (VehicycleBicycleSum _)) (Just (Entity _ (Bicycle brand))) _ =
    Bike brand
convert (Entity _ (VehicycleCarSum _)) _ (Just (Entity _ (Car make model))) =
    Car make model
convert _ =
    error "The database preconditions have been violated!"
```

The SQL table that is automatically generated from the entities does not guarantee that exactly one ID column is present.
We can resolve this by adding a `CHECK` constraint:

We need to add another check to ensure that *at most* one of the columns is present.

```sql
ALTER TABLE vehicle ADD CHECK
    ( (bicycle IS NOT NULL AND car     IS NULL)
   OR (car     IS NOT NULL AND bicycle IS NULL)
    );
```

As compared to the "Shared Primary Keys" approach, this inverts the foreign key relationship.
This means that we would need to insert an `Cat` or `Dog` into the database before we can insert an `Animal` into the database.
Where the previous method would allow "orphan" `animal` records with no corresponding `cat`, `dog`, etc, this method allows for orphan `cat`s and `dog`s without corresponding `animal` records.

Adding constructors is easy - you add a new column to the `animal` table, and adjust the `CHECK` constraints so that only one can be present.

This requires less work with adding custom `ENUM` types, requires fewer and less complicated foreign keys, and has less "dynamic" behavior (linking the `id` and `type` field at runtime vs statically known relationship).

# Nullable Columns

This approach dispenses with multiple tables and represents the sum-type in a single table.
It is an awkward encoding, and it is not considered good database design.
However, it does avoid many `JOIN`s, and is slightly more straightforward.

We will start by creating a table for `animal` and *denormalize* the other tables in, with `NULL`able columns.

```sql
CREATE TABLE animal (
    -- Common fields:
    id      SERIAL PRIMARY KEY,
    type    animal_constr NOT NULL,

    -- Cat fields:
    cat_name    TEXT,
    cat_age     INTEGER,
    cat_food    TEXT,

    -- Dog fields:
    dog_name        TEXT,
    dog_owner_id    INTEGER REFERENCES owner,
    
    -- Bird fields:
    bird_name       TEXT,
    bird_song       TEXT

);
```

Now, when we parse fields out of the database, we will look a the `type` field, and assume that the relevant fields are not null.
We can make this safe by using a `CHECK` constraint to ensure that the corresponding fields are not null, and that the irrelevant fields are null.

```sql
ALTER TABLE animal ADD CHECK (
        (type = 'cat') = (cat_name IS NOT NULL)
    AND (type = 'cat') = (cat_age IS NOT NULL)
    AND (type = 'cat') = (cat_food IS NOT NULL)
    AND (type = 'dog') = (dog_name IS NOT NULL)
    AND (type = 'dog') = (dog_owner_id IS NOT NULL)
    AND (type = 'bird') = (bird_name IS NOT NULL)
    AND (type = 'bird') = (bird_song IS NOT NULL)
);
```

This `CHECK` constraint is getting pretty gnarly, and it's only going to get worse if we add additional constructors and fields.
This kind of boilerplate can be automated away.
I suspect that a complex `CHECK` constraint like this might become computationally intensive, as well, though I have no idea what the performance characteristics of it are.

This approach is explicitly denormalized, and your DBA friends may scoff at you for implementing it.
However, it has many upsides, as well.
It is simple and easy to query, and aside from the safety `CHECK` constraints to guarantee data integrity, it is relatively low boilerplate.

If you want to provide this schema for convenience, you might consider using one of the previous two choices and exposing this as a `VIEW` on the underlying data.
The query to provide the same schema from the "Shared Primary Keys" approach is here:

```sql
CREATE VIEW animal_a (
    id      INTEGER,
    type    animal_constr NOT NULL,

    cat_name    TEXT,
    cat_age     INTEGER,
    cat_food    TEXT,

    dog_name        TEXT,
    dog_owner_id    INTEGER REFERENCES owner,
    
    bird_name       TEXT,
    bird_song       TEXT
) AS 
SELECT 
    id, type, 
    cat.name as cat_name, cat.age as cat_age, cat.food as cat_food,
    dog.name as dog_name, dog.owner_id as dog_owner_id,
    bird.name as bird_name, bird.song as bird_song
FROM animal
LEFT JOIN cat
    ON animal.id = cat.id
LEFT JOIN dog
    ON animal.id = dog.ig
LEFT JOIN bird
    ON animal.id = bird.id
WHERE cat.id IS NOT NULL
   OR dog.is IS NOT NULL
   OR bird.id IS NOT NULL
```

The view from the "Persistent" approach is slightly different, because the IDs for the cats/dogs/birds differ from the animal ID.
If you needed to provide absolute backwards compatibility here, then you could provide the view with redundant `cat_id` etc columns.

```sql
CREATE VIEW animal_b (
    id      INTEGER,
    type    animal_constr NOT NULL,

    cat_id      INTEGER
    cat_name    TEXT,
    cat_age     INTEGER,
    cat_food    TEXT,

    dog_id          INTEGER
    dog_name        TEXT,
    dog_owner_id    INTEGER REFERENCES owner,
    
    bird_id         INTEGER
    bird_name       TEXT,
    bird_song       TEXT
) AS
SELECT
    id, type, 
    cat.id as cat_id, cat.name as cat_name, cat.age as cat_age, cat.food as cat_food,
    dog.id as dog_id, dog.name as dog_name, dog.owner_id as dog_owner_id,
    bird.id as bird_id, bird.name as bird_name, bird.song as bird_song
FROM animal
LEFT JOIN cat
    ON animal.cat_id = cat.id
LEFT JOIN dog
    ON animal.dog_id = dog.id
LEFT JOIN bird
    ON animal.bird_id = bird.id
```

Since this option is expressible as a `VIEW` on the other two options, I'd suggest doing that if you need to provide this schema.

# Datatype Normalization

Above, I mentioned that the `Animal` datatype we're using is not normalized.
Haskell datatypes can be normalized in much the same way that SQL relations can be.
The [Wikipedia](https://en.wikipedia.org/wiki/Database_normalization) is a good way to learn about this, and the process is essentially the same.

First, we notice that the `Name` field is repeated in each constructor.
We are going to factor that field out.

```haskell
data AnimalDetails
    = Cat Age FavoriteFood
    | Dog OwnerId
    | Bird Song

data Animal 
    = Animal Name AnimalDetails
```

All of the repetition has been factored out.

# Addendum

[@cliffordheath](https://twitter.com/cliffordheath/status/1108497262450102272) commented that these three strategies have well-known names in the database world - absorption, separation, and partition.
