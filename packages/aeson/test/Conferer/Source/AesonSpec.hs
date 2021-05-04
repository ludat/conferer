module Conferer.Source.AesonSpec where

import Data.Aeson.QQ
import Test.Hspec

import Conferer.Source
import Conferer.Source.Aeson

spec :: Spec
spec = do
  describe "json source" $ do
    let mk = return . fromValue "dir/file.json"
    describe "#getKeyInSource" $ do
      it "getting an existing path returns the right value" $ do
        c <- mk [aesonQQ| {"postgres": {"url": "some url", "ssl": true}} |]
        res <- getKeyInSource c "postgres.url"
        res `shouldBe` Just "some url"

      it "getting an non existing path returns nothing" $ do
        c <- mk [aesonQQ| {"postgres": {"url": "some url", "ssl": true}} |]
        res <- getKeyInSource c "some.path"
        res `shouldBe` Nothing

      it "getting an non existing path returns nothing" $ do
        c <- mk [aesonQQ| {"postgres": {"url": "some url", "ssl": true}} |]
        res <- getKeyInSource c "some.path"
        res `shouldBe` Nothing

      context "getting a key with an object that has '_self'" $ do
        it "gets the value inside '_self'" $ do
          c <- mk [aesonQQ| { key: {_self: 72}} |]
          res <- getKeyInSource c "key"
          res `shouldBe` Just "72"

      context "with a key that's not valid" $ do
        it "ignores the value" $ do
          c <- mk [aesonQQ| { key: {key_: 72}} |]
          res <- getKeyInSource c "key"
          res `shouldBe` Nothing

      describe "with an array" $ do
        it "getting a path with number gets the right value" $ do
          c <- mk [aesonQQ| {"key": ["value"]} |]
          res <- getKeyInSource c "key.0"
          res `shouldBe` Just "value"
        describe "magic 'keys' key" $ do
          context "with an empy array" $ do
            it "the magic 'keys' key is \"\"" $ do
              c <- mk [aesonQQ| {"key": []} |]
              res <- getKeyInSource c "key.keys"
              res `shouldBe` Just ""
          context "with a non empty array" $ do
            it "the magic 'keys' key is a comma separated list of all \
               \indexes present on the array" $ do
              c <- mk [aesonQQ| {"key": [true, true, true]}|]
              res <- getKeyInSource c "key.keys"
              res `shouldBe` Just "0,1,2"

      describe "with an object" $ do
        it "getting an existing path returns nothing" $ do
          c <- mk [aesonQQ| {"key": { "path": "value"}}|]
          res <- getKeyInSource c "key"
          res `shouldBe` Nothing
        describe "magic 'keys' key" $ do
          context "with an empy object" $ do
            it "the magic 'keys' key is \"\"" $ do
              c <- mk [aesonQQ| {"key": {}}|]
              res <- getKeyInSource c "key.keys"
              res `shouldBe` Just ""
          context "with a non empty object" $ do
            it "the magic 'keys' key is a comma separated list of all \
               \keys present in the object" $ do
              c <- mk [aesonQQ| {"key": {a: true, b: true, c: true}}|]
              res <- getKeyInSource c "key.keys"
              res `shouldBe` Just "a,b,c"
          context "with an object that has a 'keys' key present" $ do
            it "return its value instead of doing the metamagic" $ do
              c <- mk [aesonQQ| {"key": {keys: "something", c: true}}|]
              res <- getKeyInSource c "key.keys"
              res `shouldBe` Just "something"

      describe "with an int" $ do
        it "getting an existing path returns the right value" $ do
          c <- mk [aesonQQ| {"key": 1} |]
          res <- getKeyInSource c "key"
          res `shouldBe` Just "1"

      describe "with a float" $ do
        it "getting an existing path returns the right value" $ do
          c <- mk [aesonQQ| {"key": 1.2} |]
          res <- getKeyInSource c "key"
          res `shouldBe` Just "1.2"

      describe "with a boolean" $ do
        it "getting an existing path returns the right value" $ do
          c <- mk [aesonQQ| {"key": false} |]
          res <- getKeyInSource c "key"
          res `shouldBe` Just "false"
    describe "#getSubkeysInSource" $ do
      describe "listing keys in non existant path" $ do
        it "gets no keys" $ do
          c <- mk [aesonQQ|{}|]
          res <- getSubkeysInSource c "a"
          res `shouldBe` []
      describe "listing keys with an empty object" $ do
        it "gets only the 'keys' magic key" $ do
          c <- mk [aesonQQ|{}|]
          res <- getSubkeysInSource c ""
          res `shouldBe` []
      describe "gettings a existing subkey directly" $ do
        it "gets no keys" $ do
          c <- mk [aesonQQ|{"key": 7}|]
          res <- getSubkeysInSource c "key"
          res `shouldBe` []
      describe "getting the subkeys for an object" $ do
        it "gets the keys" $ do
          c <- mk [aesonQQ|{"key": 7}|]
          res <- getSubkeysInSource c ""
          res `shouldBe` ["key"]
      describe "gettings the keys from a list and an object" $ do
        it "gets the keys as index numbers" $ do
          c <- mk [aesonQQ|{"a": ["a"]}|]
          res <- getSubkeysInSource c ""
          res `shouldBe` ["a.0"]
      describe "getting the keys from subkeys" $ do
        it "gets no keys" $ do
          c <- mk [aesonQQ|{"a": ["a"]}|]
          res <- getSubkeysInSource c "a"
          res `shouldBe` ["a.0"]
      describe "gettings the keys from nested objects" $ do
        it "gets those keys and all intermediate magic 'keys' keys" $ do
          c <- mk [aesonQQ|{"a": {"a": 7}}|]
          res <- getSubkeysInSource c ""
          res `shouldBe` ["a.a"]
      describe "gettings the keys from list" $ do
        it "returns the present indexes" $ do
          c <- mk [aesonQQ|[true, true, true]|]
          res <- getSubkeysInSource c ""
          res `shouldBe` ["0", "1", "2"]
      describe "gettings keys with invalid names" $ do
        it "ignores those names" $ do
          c <- mk [aesonQQ|{"_a": 7}|]
          res <- getSubkeysInSource c ""
          res `shouldBe` []
      describe "when json keys don't follow the conferer Key format" $ do
        it "ignores the evil keys" $ do
          c <- mk [aesonQQ|{some: {k_e_y: 0}}|]
          res <- getSubkeysInSource c ""
          res `shouldBe` []
      describe "when '_self' is present" $ do
        it "gets an object if it has '_self'" $ do
          c <- mk [aesonQQ|{some: {_self: 7, key: 0}}|]
          res <- getSubkeysInSource c ""
          res `shouldBe` ["some", "some.key"]
    describe "#invalidJsonKeys" $ do
      context "with some common keys" $
        it "works" $ do
          invalidJsonKeys [aesonQQ|{postgres: {url: "some url", ssl: true}} |]
            `shouldBe` []
      context "with one top level invalid key" $
        it "fails" $ do
          invalidJsonKeys [aesonQQ|{k_e_y: {}} |]
            `shouldBe` [["k_e_y"]]
      context "with one invalid key inside an object" $
        it "fails" $ do
          invalidJsonKeys [aesonQQ|{some: {k_e_y: {}}} |]
            `shouldBe` [["some", "k_e_y"]]
      context "with one invalid key inside an array" $
        it "fails" $ do
          invalidJsonKeys [aesonQQ|[{k_e_y: {}}]|]
            `shouldBe` [["0", "k_e_y"]]
      context "with _self" $
        it "succeeds" $ do
          invalidJsonKeys [aesonQQ|[{some: {_self: 7}}]|]
            `shouldBe` []
      context "with empty key" $
        it "fails" $ do
          invalidJsonKeys [aesonQQ|{"": 7}|]
            `shouldBe` [[""]]
    describe "#explainNotFound" $ do
      context "When there is an on root that's missing a key" $ do
        it "recomends adding that key mentioning 'the root object'" $ do
          let s = fromValue "file.json" [aesonQQ|{}|]
          explainNotFound s "port"
            `shouldBe` "Replacing the whole json from '{}' to '{\"port\":\"some value\"}' on file 'file.json'"

      context "When there is an object that's missing a key" $ do
        it "recomends adding that key that's necessary" $ do
          let s = fromValue "file.json" [aesonQQ|{server: {}}|]
          explainNotFound s "server.port"
            `shouldBe` "Replacing the value at 'server' from '{}' to '{\"port\":\"some value\"}' on file 'file.json'"

      context "When there is an object in the key" $ do
        it "recomends replacing it with a value" $ do
          let s = fromValue "file.json" [aesonQQ|{port: {}}|]
          explainNotFound s "port"
            `shouldBe` "Replacing the value at 'port' from '{}' to '\"some value\"' on file 'file.json'"

      context "When there is a primitive somewhere in the middle" $ do
        it "recommends using '_self' and adding the key" $ do
          let s = fromValue "file.json" [aesonQQ|{server: 7}|]
          explainNotFound s "server.port"
            `shouldBe` "Replacing the value at 'server' from '7' to '{\"_self\":7,\"port\":\"some value\"}' on file 'file.json'"

      context "When the key ends with `keys`" $ do
        it "recommends adding an object beside adding the raw key" $ do
          let s = fromValue "file.json" [aesonQQ|{}|]
          explainNotFound s "servers.keys"
            `shouldBe` "Replacing the whole json from '{}' to '{\"servers\":{\"keys\":\"some value\"}}' on file 'file.json'"

      context "When the key has an array" $ do
        it "recommends turning the array into an object and using self" $ do
          let s = fromValue "file.json" [aesonQQ|{"port": []}|]
          explainNotFound s "port"
            `shouldBe` "Replacing the value at 'port' from '[]' to '\"some value\"' on file 'file.json'"

      context "When the key has an array with values" $ do
        it "recommends turning the array into an object and using self (adding existing keys)" $ do
          let s = fromValue "file.json" [aesonQQ|{"port": [false]}|]
          explainNotFound s "port"
            `shouldBe` "Replacing the value at 'port' from '[false]' to '{\"0\":false,\"_self\":\"some value\"}' on file 'file.json'"

      context "When the _self key has an object" $ do
        it "recommends replacing it with some value" $ do
          let s = fromValue "file.json" [aesonQQ|{"port": {"_self": {}}}|]
          explainNotFound s "port"
            `shouldBe` "Replacing the value at 'port._self' from '{}' to '\"some value\"' on file 'file.json'"

      context "When the _self key has an array" $ do
        it "recommends replacing it with some value" $ do
          let s = fromValue "file.json" [aesonQQ|{"port": {"_self": []}}|]
          explainNotFound s "port"
            `shouldBe` "Replacing the value at 'port._self' from '[]' to '\"some value\"' on file 'file.json'"
    describe "#explainSettedKey" $ do
      context "with a simple value" $ do
        it "returns its path" $ do
          let s = fromValue "file.json" [aesonQQ|{"port": 9999}|]
          explainSettedKey s "port"
            `shouldBe` "json key 'port' on file 'file.json'"
      context "with a simple value inside an array" $ do
        xit "returns its path" $ do
          let s = fromValue "file.json" [aesonQQ|{"port": [9999]}|]
          explainSettedKey s "port.0"
            `shouldBe` "json key 'port[0]' on file 'file.json'"
          -- Here and in every test I'd like to user the more normal
          -- notation for paths (the one from js)
      context "with an object with _self" $ do
        it "returns its real path (including _self)" $ do
          let s = fromValue "file.json" [aesonQQ|{"port": {"_self": 9999}}|]
          explainSettedKey s "port"
            `shouldBe` "json key 'port._self' on file 'file.json'"
