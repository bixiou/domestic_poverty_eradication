# OFF-BANK PAYMENT SYSTEM VIA TAXISnet
## Me.F.E. Units (Future Tax Exemptions in Euro)

Yanis Varoufakis
*November 2014*

<!-- English translation of Greece_plan_B.pdf (Greek original transcribed in Greece_plan_B.md). "ΜεΦΕ" (Μελλοντικές Φοροαπαλλαγές σε Ευρώ) is rendered "FTE" (Future Tax Exemptions). TAXISnet is the Greek tax authority's online platform; ΑΦΜ = tax identification number (TIN). Page numbers in comments refer to the document's own numbering. -->

---

## Preface

The aims of the proposed system are:

1. Injecting liquidity into the public sector – something particularly useful during a period of tough negotiation with the troika
2. Injecting liquidity into the private sector by creating a new payment system, in euro, outside the banking system and open to all citizens for transactions among themselves at zero cost
3. Minimising the social cost of a possible (though avoidable) currency conversion – a minimisation that increases the country's bargaining power and thus reduces the risk of such a conversion

The idea is simple: all taxpayers can today transfer (via Web Banking) sums from their bank accounts to a special TAXISnet account, thereby paying off their debts to the Tax Office.

It is technically and legally a simple matter for these TAXISnet accounts to be converted into reserve-debit accounts:

- into which the State can pay part of its debts to private parties in accounting units called FTE (see below for their definition), each corresponding to (at least) one euro
- from which private parties can settle their debts to one another, as well as their debts to the State

**FTE (Definition):** An accounting unit that is issued or purchased at 1 euro each in period t but which, in period t+1, extinguishes taxes or other obligations to the State worth (1+ρ) – where 0<ρ<1

The resulting reserve accounts (in FTE) will contain balances arising from:

- amounts the State transferred, in FTE, on account of its arrears to the private party in question
- amounts private parties purchased, receiving FTE in exchange for "regular" euro, at a purchase price of 1 euro per 1 FTE, with the aim of reducing their future taxes.

<!-- p. 2 -->

Finally, FTE that remain unused in the taxpayer's reserve-debit account until the end of the year following their issue are automatically increased (by θ, where 0<θ<1), so that private parties have an additional incentive to delay using FTE to pay taxes in the current period.

As shown below, such a system is fiscally sustainable and capable of achieving the three aims 1, 2 & 3 above.

<!-- p. 3 -->

## 1. Preliminaries: Creating reserve-debit accounts (for each TIN) in TAXISnet

Today every taxpayer (every TIN) potentially has a debit account in TAXISnet into which they pay the taxes they owe by making a transfer (via Web Banking) from their bank account to their TAXISnet debit account.

It is proposed that this debit account be extended in three directions:

1. The State should be able to pay specific arrears owed to a taxpayer, e.g. a Company A, into its TAXISnet debit account, thereby turning it into a reserve-debit account (current reserve account).
2. Company A should be able to choose how to use these reserves, among:
   a. using them to pay, immediately or in the future, its debts to the State (income tax, VAT, road tax, fines, contributions, etc.)
   b. using them to pay its debts to third parties by crediting their reserve-debit account – through the TAXISnet web application (a simple adaptation of the existing system)
   c. keeping them in that account as an investment product (e.g. a form of bond).

For example, see the diagram below: suppose the State owes Company A arrears of €200,000. Suppose further that Company A owes €50,000 to Company B and a salary of €2,000 to Employee 1. Meanwhile, Company B owes more than €48,000 to the Tax Office and €2,000 to Employee 2, who in turn owes €800 to Citizen 3, who also has to pay more than €800 in taxes to the State.

By transferring €200,000 in the form of FTE (Future Tax Exemptions in Euro) into A's reserve-debit account (without having to raise that amount either through regular borrowing or through harsher austerity), the State is released from this sum for the current fiscal year. At the same time, however, private-sector liquidity increases, because the ability to transfer FTE from one private party's reserve-debit account to another's (from Company A to B, to their employees and, generally, to other citizens) clears many debts among private parties and to the State. In the example above, we have the following balance sheets:

<!-- p. 4 -->

*[Flow diagram:]*
- STATE → COMPANY A: 200 thousand
- COMPANY A → STATE: 40,000
- COMPANY A → COMPANY B: 50 thousand
- COMPANY A → EMPLOYEE 1: 2,000
- COMPANY B → STATE: 48,000
- COMPANY B → EMPLOYEE 2: 2,000
- EMPLOYEE 1 → STATE: 300
- EMPLOYEE 2 → STATE: 300
- EMPLOYEE 2 → CITIZEN 3: 800
- CITIZEN 3 → STATE: 800

| Company A | Company B | Empl. 1 | Empl. 2 | Citizen 3 |
|---|---|---|---|---|
| +200,000 (from State) | +50,000 (from A) | +2,000 (from A) | +2,000 (from A) | +800 (from 2) |
| -50,000 (to B) | -48,000 (taxes to State) | -300 (taxes to State) | -300 (taxes to State) | -800 (taxes to State) |
| -40,000 (taxes to State) | -2,000 (to 2) | | -800 (to 3) | |
| -2,000 (to 1) | | | | |
| **Balance = 108,000** | **Balance = 0** | **Balance = 1,700** | **Balance = 900** | **Balance = 0** |

<!-- Note: the original table says Empl. 2 receives +2,000 "(from A)"; per the text and diagram it comes from B. -->

**STATE ACCOUNT**

| LIABILITIES | TAX WRITE-OFFS |
|---|---|
| +200,000 | -40,000 (write-off of A's taxes) |
| | -48,000 (write-off of B's taxes) |
| | -300 (write-off of Empl. 1's taxes) |
| | -300 (write-off of Empl. 2's taxes) |
| | -800 (write-off of Citizen 3's taxes) |
| Total = 200,000 | Total = 99,400 |

Final total of State debts rolled over to the next year = 200,000 – 99,400 = 100,600

In other words, the initial €200,000 that the State does ***not*** pay Company A in cash becomes a reserve asset denominated in FTE, circulating within the "closed" payment system that is a simple extension of TAXISnet. The account-holding taxpayers use these reserves for payments among themselves and to pay taxes to the State. FTE function as digital euro that "extinguish" debts to the State, and which "live" inside TAXISnet, allowing their private holders to settle private debts to one another as well.

<!-- p. 5 -->

## 2. Benefits to the State and private parties: Provision for future tax discounts

The State's net benefit in the previous example is that in the current year it has reduced its liquidity needs by 50.3% (it paid out, in the form of lost cash tax revenue, €99,400 instead of €200,000). This benefit, however, becomes significant to the extent that citizens and businesses acquire a strong incentive to keep FTE in their (TAXISnet) account rather than redeeming them against debts to the State as quickly as possible.

A simple way to give private parties such an incentive to hold on to FTE is to legislate that:

**Future tax discounts:** FTE not used to pay off taxes for at least one year can "extinguish" debts to the State equal to (1+ρ) times their nominal amount (where 0<ρ<1).

E.g. if ρ=0.1, 100 FTE units issued today will, in one year, be able to "extinguish" 110 units of debt to the State. In effect, the State rewards the private party who did not reduce their payments to the State by using their FTE, but instead waited at least one year, with a (not insignificant) tax discount of around 10%. Suddenly, the private party has an incentive even to buy FTE units (if they do not have any) in order to obtain, in one year, a 10% tax discount!

Moreover, if these FTE units are transferable (from one private party to another) at a freely negotiated price, then FTE units evolve into a new parallel currency, with an exchange value of 1 FTE to 1 euro, but likely to sell at prices above their nominal value: e.g. when the end of fiscal year 2015 arrives, Citizen 1, who has no FTE issued in 2014, may be willing to buy Citizen 2's 2014-issued FTE units at a "premium" of, say, 5% – since they secure a 10% tax discount. And why would Citizen 2 sell? Because they may have bought these FTE as an investment "product" – i.e. to sell them at a profit!

<!-- p. 6 -->

In short, the State's benefit (reduction of its euro debts) from the FTE electronic payment system becomes significant provided FTE can "extinguish" future tax liabilities larger than their nominal value (i.e. provided they carry the legal right to future tax exemptions). Then the relief of the State's current obligations to private parties is maximised while, at the same time, private parties who buy FTE as an investment instrument are attracted into the system, further increasing the State's liquidity and simultaneously strengthening liquidity within the parallel FTE payment system.

## 3. Institutional measures

For this new system to work, the following are required:

1. Opening a debit-reserve account in TAXISnet for every TIN – without any application by the holders
2. Opening a debit-reserve account in TAXISnet for any Greek or foreign individual or institution wishing to buy FTE by paying cash to the Greek State – with a simple application
3. Creating an electronic clearing house (secondary clearing market) where FTE holders can sell them (at prices set by the free FTE market) to anyone wishing to acquire FTE in exchange for euro
4. Each FTE carries a digital timestamp with its issue date – so that its holder (or prospective buyer) knows when the tax exemption provided by law becomes active
5. A provision/commitment in the State Budget for year t specifying the tax-exemption rate (ρ) to which FTE units issued during year t are entitled for tax payments in year t+1

<!-- p. 7 -->

## 4. The political-economic dimension: A summary

The proposed system gives:

- the State access to a new form of borrowing (outside the bond markets, dependence on the banks, and the constraints of the EU, Maastricht and any kind of supervision), *and*
- the economy a new payment and liquidity system that goes beyond and bypasses the banks.

This borrowing will initially be mostly compulsory (e.g. arrears paid in FTE instead of regular euro) but increasingly voluntary (e.g. private parties choosing to exchange euro for FTE, with a view to future tax exemptions).

The payment system among private parties will have the great advantage of fully bypassing the banking system, with a significant reduction in transaction costs for private parties. Within a short time, these transactions can be made with a simple mobile phone APP, at no cost to either the State or the users.

The political benefit of such a system is multidimensional:

- It gives the government degrees of freedom during a tough negotiation
- It frees up "regular" euro of both the public sector and private parties
- It reduces transaction costs between the State and private parties (and among private parties)
- It reduces the bankers' monopoly over society and over the payment system on which economic life rests
- It creates a parallel electronic/online euro which, unlike the "other" euro, can – at the push of a button, in an emergency – be converted into another currency

<!-- p. 8 -->

## APPENDIX
### Fiscal stability: Algebraic analysis

Suppose that initially, in period t=0, the State injects X FTE into this new payment system (either because it sold them to private parties or because it converted debts of its own into FTE).

Suppose further that the tax-exemption rate achieved in period t+1 by each FTE issued in year t equals ρ (where 0<ρ<1), while the share of FTE redeemed each year equals α (where 0<α<1).

| Period/year | Increase in State liquidity | State tax losses |
|---|---|---|
| t=0 | X | - |
| t=1 | αX(1+ρ) | (1-α)X |
| t=2 | α[(1-α)X](1+ρ) | (1-α)²X |
| t=3 | α[(1-α)²X](1+ρ) | (1-α)³X |
| t=4 | α[(1-α)³X](1+ρ) | (1-α)⁴X |

<!-- Note: column headers/entries appear swapped relative to the numerical tables below (as in the original). -->

If, e.g., the tax exemption of each FTE equals 10% (ρ=0.1) and 80% of FTE are redeemed in their year of issue (α=0.8), it is clear that the 200 thousand euro the State paid Company A in FTE (see example above) evolve over time as follows:

| Period/year | Increase in State liquidity | State tax loss |
|---|---|---|
| t=0 | 200,000 | - |
| t=1 | 40,000 | 176,000 |
| t=2 | 8,000 | 35,200 |
| t=3 | 1,600 | 7,040 |
| t=4 | 320 | 1,408 |
| t=5 | 64 | 282.6 |
| t=6 | 12.8 | 51.3 |

*ρ=0.1 and α=0.8*

Put simply, the State avoids paying 200 thousand euro now by rolling its obligation over to the next year, at an "interest rate" of 10%. However, since 20% (1-α=0.2) of FTE are not used in period t=1 and are rolled over another year, in period t=1 the State loses 176 thousand instead of 220 thousand, and the total interest of this form of borrowing (the 20 thousand euro) is "spread" over the following years in the way shown by the first column of the table above.

Obviously, it is in the government's interest that private parties delay redeeming their FTE – i.e. that α be small – see in the

<!-- p. 9 -->

next table, where α=0.5, how the State successfully pushes its "costs" into the future.

| Period/year | Increase in State liquidity | State tax loss |
|---|---|---|
| t=0 | 200,000 | - |
| t=1 | 100,000 | 110,000 |
| t=2 | 50,000 | 55,000 |
| t=3 | 25,000 | 27,500 |
| t=4 | 12,500 | 13,750 |
| t=5 | 6,250 | 6,875 |
| t=6 | 3,125 | 3,437 |

*ρ=0.1 and α=0.5*

At this point it should be noted that the values of ρ and α are related to each other – since a large ρ gives FTE holders an incentive not to redeem them immediately (reducing α), but to keep them (or sell them to other private parties who will hold them) in order to benefit from the large tax exemption, equal to ρ.

On the other hand, a large tax exemption ρ endangers the sustainability of the FTE payment system. The question is therefore twofold:

- <u>How many FTE should the State issue annually?</u>
- <u>And what tax-exemption rate ρ should FTE offer their holders?</u>

In each period t, a share α of FTE is redeemed. For the quantity of FTE to remain constant, the quantity x<sub>t+1</sub> of ***new*** FTE must equal the share α of all FTE X<sub>t</sub> in circulation in period t. In general, condition (1) must hold, where g<sub>t+1</sub> is the rate of change of the tax base in period t+1:

x<sub>t+1</sub> = αX<sub>t</sub> + (1+g<sub>t+1</sub>)(1-α)X<sub>t</sub> = X<sub>t</sub> [α+(1+g<sub>t+1</sub>)(1-α)]  (1)

Consequently, the quantity of FTE in period t+1 is given by equation (2):

X<sub>t+1</sub> = X<sub>t</sub> - αX<sub>t</sub> + αX<sub>t</sub> + (1+g<sub>t+1</sub>)(1-α)X<sub>t</sub> = X<sub>t</sub> + (1+g<sub>t+1</sub>)(1-α)X<sub>t</sub>  (2)

while the growth rate of the quantity of FTE is given by:

θ = (X<sub>t+1</sub>-X<sub>t</sub>)/X<sub>t</sub> = (1+g)(1-α)  (3)

More simply, if α=1 (i.e. all FTE are redeemed in their year of issue), then the stock of FTE stays constant whatever happens to the tax base. If, however, α<1 (i.e. some FTE are saved), then the

<!-- p. 10 -->

stock of FTE varies with the size of the tax base. E.g. if 20% of FTE are saved (α=0.8) and the tax base grows by 3% a year, then the quantity of FTE grows by 0.2×1.03 = 0.206, i.e. by 20.6% a year.

Finally, an additional incentive for FTE to be held rather than redeemed would be the following: every FTE issued in period t that is not redeemed in period t+1 is automatically replaced by (1+θ) new FTE issued in period t+1, credited to the holder's account. We thus have two incentives for someone to want FTE: first, they allow a discount ρ on next year's tax and, second, not using them earns interest at rate θ on the holder's FTE account. Attractive values of ρ and θ will keep the redemption rate α low, resulting in a significant increase in the liquidity of (A) the parallel payment system and (B) the State. Finally, provided equations (1), (2) and (3) hold, the proposed system is fiscally neutral.
