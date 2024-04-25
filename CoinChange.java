import java.util.Arrays;

public class CoinChange {
    private int[] denominations;

    public CoinChange(int[] denominations) {
        Arrays.sort(denominations);
        this.denominations = denominations;
    }

    public void findMinCoins(int amount) {
        for (int i = denominations.length - 1; i >= 0; i--) {
            while (amount >= denominations[i]) {
                System.out.print(denominations[i] + " ");
                amount -= denominations[i];
            }
        }
    }

    public static void main(String[] args) {
        int[] coinDenominations = {50, 25, 10, 5, 1};
        CoinChange coinChange = new CoinChange(coinDenominations);
        coinChange.findMinCoins(100); // Example usage: finding minimum coins for amount 100
    }
}