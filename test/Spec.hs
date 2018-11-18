import Test.Hspec
import NumConv

main :: IO ()
main = hspec $ do
    --Рабочий диапазон: входное число - целое неотрицательное,
    --основание системы - натуральное от 2 до 36
    describe "Проверка в рабочем диапазоне" $ do
        it "Из строки в заданном основании в число" $ do
            convertFrom 10 "1234" `shouldBe` Just 1234
            convertFrom 16 "ff" `shouldBe` Just 255
        it "Из строки обоих регистрах" $ do
            convertFrom 16 "FF" `shouldBe` Just 255
            convertFrom 16 "fFfF" `shouldBe` Just 65535
        it "Из числа в строку в заданном основании" $ do
            convertTo 10 1234 `shouldBe` Just "1234"
            convertTo 16 15 `shouldBe` Just "f"
        it "Из одного основания в другое" $ do
            convertFromTo 10 16 "16" `shouldBe` Just "10"
            convertFromTo 16 10 "16" `shouldBe` Just "22"
    describe "Проверка на границах рабочего диапазона" $ do
        it "Нули" $ do
            convertFrom 10 "0" `shouldBe` Just 0
            convertFrom 2 "0" `shouldBe` Just 0
        it "Двоичные числа (минимальное основание)" $ do
            convertFromTo 2 10 "10000" `shouldBe` Just "16"
            convertFromTo 10 2 "255" `shouldBe` Just "11111111"
        it "Максимальное основание" $ do
            convertFromTo 36 10 "14" `shouldBe` Just "40"
            convertFromTo 10 36 "1295" `shouldBe` Just "zz"
    describe "Проверка вне границ рабочего диапазона" $ do
        it "Неправильно записанные числа" $ do
            convertFrom 4 "1234" `shouldBe` Nothing
            convertFrom 10 "12ab" `shouldBe` Nothing
        it "Отрицательные числа" $ do
            convertFrom 4 "-123" `shouldBe` Nothing
            convertTo 8 (-12) `shouldBe` Nothing
        it "Недопустимое основание" $ do
            convertFrom 1 "1234" `shouldBe` Nothing
            convertFrom 37 "12ab" `shouldBe` Nothing
            convertTo 37 256 `shouldBe` Nothing
