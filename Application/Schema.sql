CREATE EXTENSION IF NOT EXISTS "uuid-ossp";
CREATE TABLE articles (
    id UUID DEFAULT uuid_generate_v4() PRIMARY KEY NOT NULL UNIQUE,
    name TEXT NOT NULL,
    price REAL NOT NULL,
    volume REAL NOT NULL,
    item_group TEXT NOT NULL,
    style TEXT NOT NULL,
    abv REAL NOT NULL,
    availability TEXT NOT NULL,
    apk REAL NOT NULL,
    origin_id INT NOT NULL
);
