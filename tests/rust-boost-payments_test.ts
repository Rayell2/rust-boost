import { Clarinet, Tx, Chain, Account, types } from 'https://deno.land/x/clarinet@v1.0.0/index.ts';
import { assertEquals } from 'https://deno.land/std@0.170.0/testing/asserts.ts';

Clarinet.test({
    name: "Rust Boost Payments: Create Task",
    async fn(chain: Chain, accounts: Map<string, Account>) {
        const deployer = accounts.get('deployer')!;
        const participant1 = accounts.get('wallet_1')!;
        const participant2 = accounts.get('wallet_2')!;

        const taskAmount = 1000000; // 1 STX

        const block = chain.mineBlock([
            Tx.contractCall('rust-boost-payments', 'create-task', 
                [types.principal(participant2.address), types.uint(taskAmount)],
                participant1.address
            )
        ]);

        assertEquals(block.height, 2);
        block.receipts[0].result.expectOk().expectUint(1);
    }
});

Clarinet.test({
    name: "Rust Boost Payments: Send Tip",
    async fn(chain: Chain, accounts: Map<string, Account>) {
        const deployer = accounts.get('deployer')!;
        const participant1 = accounts.get('wallet_1')!;
        const participant2 = accounts.get('wallet_2')!;

        const tipAmount = 500000; // 0.5 STX

        const block = chain.mineBlock([
            Tx.contractCall('rust-boost-payments', 'send-tip', 
                [types.principal(participant2.address), types.uint(tipAmount)],
                participant1.address
            )
        ]);

        assertEquals(block.height, 2);
        block.receipts[0].result.expectOk();
    }
});