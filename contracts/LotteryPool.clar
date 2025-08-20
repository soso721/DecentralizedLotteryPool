;; File: contracts/LotteryPool.clar

;; Define our own block-info trait for testing
(define-trait block-info (
    (get-block-info?
        (uint)
        (response (buff 32) uint)
    )
))

;; -----------------------------------------------------------
;; Decentralized Lottery (Commit-Reveal, Rollover, Fee)
;; -----------------------------------------------------------
;; Flow:
;; - Admin creates rounds with ticket price, max tickets, reveal window, fee bps.
;; - Players buy tickets by committing hash(secret || salt || player || round-id).
;; - After sales close, players reveal their secrets.
;; - Random seed = hash mixing of all reveals with round entropy.
;; - Winner index = seed mod ticket-count; payout = pot - fee; fee to treasury.
;; - If no valid reveals, pot rolls over to next round.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 
;; Errors
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 
(define-constant ERR-NOT-ADMIN (err u100))
(define-constant ERR-ROUND-NOT-FOUND (err u101))
(define-constant ERR-ROUND-NOT-OPEN (err u102))
(define-constant ERR-ROUND-NOT-REVEAL (err u103))
(define-constant ERR-ROUND-ALREADY-DRAWN (err u104))
(define-constant ERR-INVALID-PARAMS (err u105))
(define-constant ERR-TICKETS-SOLD-OUT (err u106))
(define-constant ERR-INVALID-COMMIT (err u107))
(define-constant ERR-ALREADY-COMMITTED (err u108))
(define-constant ERR-NO-COMMIT (err u109))
(define-constant ERR-ALREADY-REVEALED (err u110))
(define-constant ERR-NOTHING-TO-CLAIM (err u111))
(define-constant ERR-TOO-EARLY (err u112))
(define-constant ERR-TOO-LATE (err u113))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 
;; State
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 
(define-data-var admin principal tx-sender)
(define-data-var treasury principal tx-sender)
(define-data-var round-id uint u0)
(define-data-var rollover uint u0)

(define-map rounds
    uint
    {
        ticket-price: uint,
        max-tickets: uint,
        reveal-begins: uint,
        reveal-ends: uint,
        fee-bps: uint,
        sold: uint,
        pot: uint,
        seed: (buff 32),
        drawn: bool,
        winner: (optional principal),
        created: uint,
    }
)

(define-map tickets
    {
        round: uint,
        idx: uint,
    }
    { player: principal }
)

(define-map commits
    {
        round: uint,
        player: principal,
    }
    {
        commitment: (buff 32),
        revealed: bool,
    }
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 
;; Events (using print statements)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 
(define-private (emit-round-opened
        (rid uint)
        (ticket-price uint)
        (max-tickets uint)
        (sale-end uint)
        (reveal-end uint)
        (fee-bps uint)
    )
    (print {
        event: "round-opened",
        round-id: rid,
        ticket-price: ticket-price,
        max-tickets: max-tickets,
        sale-ends: sale-end,
        reveal-ends: reveal-end,
        fee-bps: fee-bps,
        block: stacks-block-height,
    })
)

(define-private (emit-rollover-added
        (rid uint)
        (amount uint)
    )
    (print {
        event: "rollover-added",
        round-id: rid,
        amount: amount,
        block: stacks-block-height,
    })
)

(define-private (emit-ticket-bought
        (rid uint)
        (idx uint)
        (player principal)
    )
    (print {
        event: "ticket-bought",
        round-id: rid,
        ticket-index: idx,
        player: player,
        block: stacks-block-height,
    })
)

(define-private (emit-revealed
        (rid uint)
        (player principal)
    )
    (print {
        event: "revealed",
        round-id: rid,
        player: player,
        block: stacks-block-height,
    })
)

(define-private (emit-drawn
        (rid uint)
        (winner principal)
        (payout uint)
        (fee uint)
    )
    (print {
        event: "drawn",
        round-id: rid,
        winner: winner,
        payout: payout,
        fee: fee,
        block: stacks-block-height,
    })
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 
;; Helpers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 
(define-private (only-admin)
    (if (is-eq tx-sender (var-get admin))
        (ok true)
        ERR-NOT-ADMIN
    )
)

(define-private (get-round (rid uint))
    (ok (unwrap! (map-get? rounds rid) ERR-ROUND-NOT-FOUND))
)

(define-private (within-sale (rid uint))
    (match (get-round rid)
        ok-val (and (< stacks-block-height (get reveal-begins ok-val)) (not (get drawn ok-val)))
        err-val
        false
    )
)

(define-private (within-reveal (rid uint))
    (match (get-round rid)
        ok-val (and
            (>= stacks-block-height (get reveal-begins ok-val))
            (<= stacks-block-height (get reveal-ends ok-val))
            (not (get drawn ok-val))
        )
        err-val
        false
    )
)

(define-private (hash-commit
        (secret (buff 32))
        (salt (buff 32))
        (player principal)
        (rid uint)
    )
    (sha256 (concat secret
        (concat salt
            (concat (unwrap-panic (to-consensus-buff? player)) (uint-to-buff rid))
        )))
)

(define-private (mix-seed
        (old (buff 32))
        (addition (buff 32))
    )
    ;; Mix seeds using sha256(old || addition)
    (sha256 (concat old addition))
)

(define-private (uint-to-buff (x uint))
    ;; Convert uint to 8-byte buffer for consistent hashing
    (unwrap-panic (to-consensus-buff? x))
)

;; Convert buffer to uint using first 16 bytes
(define-private (buff-to-uint (b (buff 32)))
    (let ((slice (unwrap-panic (as-max-len? (unwrap-panic (slice? b u0 u16)) u16))))
        (buff-to-uint-le slice)
    )
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 
;; Admin Functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 
(define-public (set-treasury (who principal))
    (begin
        (try! (only-admin))
        (var-set treasury who)
        (ok true)
    )
)

(define-public (open-round
        (ticket-price uint)
        (max-tickets uint)
        (sale-duration uint)
        (reveal-duration uint)
        (fee-bps uint)
    )
    (begin
        (try! (only-admin))
        (asserts!
            (and
                (> ticket-price u0)
                (> max-tickets u0)
                (> sale-duration u0)
                (> reveal-duration u0)
                (<= fee-bps u1000)
            )
            ERR-INVALID-PARAMS
        )
        (let (
                (rid (+ (var-get round-id) u1))
                (sale-end (+ stacks-block-height sale-duration))
                (reveal-end (+ (+ stacks-block-height sale-duration) reveal-duration))
                (initial-seed 0x0000000000000000000000000000000000000000000000000000000000000000)
            )
            (var-set round-id rid)
            (map-set rounds rid {
                ticket-price: ticket-price,
                max-tickets: max-tickets,
                reveal-begins: sale-end,
                reveal-ends: reveal-end,
                fee-bps: fee-bps,
                sold: u0,
                pot: (var-get rollover),
                seed: (sha256 initial-seed),
                drawn: false,
                winner: none,
                created: stacks-block-height,
            })
            (begin
                (if (> (var-get rollover) u0)
                    (begin
                        (emit-rollover-added rid (var-get rollover))
                        (var-set rollover u0)
                    )
                    (var-set rollover u0)
                )
                (emit-round-opened rid ticket-price max-tickets sale-end
                    reveal-end fee-bps
                )
                (ok rid)
            )
        )
    )
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 
;; Tickets & Commit-Reveal
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 
(define-public (buy-ticket
        (rid uint)
        (commitment (buff 32))
    )
    (let ((r (try! (get-round rid))))
        (asserts! (within-sale rid) ERR-ROUND-NOT-OPEN)
        (asserts! (< (get sold r) (get max-tickets r)) ERR-TICKETS-SOLD-OUT)
        (asserts!
            (is-none (map-get? commits {
                round: rid,
                player: tx-sender,
            }))
            ERR-ALREADY-COMMITTED
        )
        (try! (stx-transfer? (get ticket-price r) tx-sender (as-contract tx-sender)))
        (map-set commits {
            round: rid,
            player: tx-sender,
        } {
            commitment: commitment,
            revealed: false,
        })
        (map-set tickets {
            round: rid,
            idx: (get sold r),
        } { player: tx-sender }
        )
        (map-set rounds rid
            (merge r {
                sold: (+ (get sold r) u1),
                pot: (+ (get pot r) (get ticket-price r)),
            })
        )
        (emit-ticket-bought rid (get sold r) tx-sender)
        (ok true)
    )
)

(define-public (reveal
        (rid uint)
        (secret (buff 32))
        (salt (buff 32))
    )
    (let ((r (try! (get-round rid))))
        (asserts! (within-reveal rid) ERR-ROUND-NOT-REVEAL)
        (match (map-get? commits {
            round: rid,
            player: tx-sender,
        })
            c (begin
                (asserts! (not (get revealed c)) ERR-ALREADY-REVEALED)
                (let (
                        (expected (get commitment c))
                        (calc (hash-commit secret salt tx-sender rid))
                    )
                    (asserts! (is-eq expected calc) ERR-INVALID-COMMIT)
                    (let ((new-seed (mix-seed (get seed r) (sha256 (concat secret salt)))))
                        (map-set rounds rid (merge r { seed: new-seed }))
                        (map-set commits {
                            round: rid,
                            player: tx-sender,
                        } {
                            commitment: expected,
                            revealed: true,
                        })
                        (emit-revealed rid tx-sender)
                        (ok true)
                    )
                )
            )
            ERR-NO-COMMIT
        )
    )
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 
;; Draw & Payout
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 
(define-public (draw
        (rid uint)
        (block-info-trait <block-info>)
    )
    (let ((r (try! (get-round rid))))
        (asserts! (>= stacks-block-height (get reveal-ends r)) ERR-TOO-EARLY)
        (asserts! (not (get drawn r)) ERR-ROUND-ALREADY-DRAWN)
        (let ((sold (get sold r)))
            (if (<= sold u0)
                ;; No tickets sold, rollover the pot
                (begin
                    (var-set rollover (+ (var-get rollover) (get pot r)))
                    (map-set rounds rid (merge r { drawn: true }))
                    (ok false)
                )
                ;; Select winner and payout
                (let (
                        (seed (get seed r))
                        ;; Add block hash entropy to prevent manipulation
                        (block-hash (unwrap-panic (contract-call? block-info-trait get-block-info?
                            (- stacks-block-height u1)
                        )))
                        (final-seed (sha256 (concat seed block-hash)))
                        (idx (mod (buff-to-uint final-seed) sold))
                    )
                    (match (map-get? tickets {
                        round: rid,
                        idx: idx,
                    })
                        t
                        (let (
                                (winner (get player t))
                                (fee (/ (* (get pot r) (get fee-bps r)) u10000))
                                (payout (- (get pot r) fee))
                            )
                            ;; Transfer fee to treasury
                            (try! (stx-transfer? fee (as-contract tx-sender)
                                (var-get treasury)
                            ))
                            ;; Transfer payout to winner
                            (try! (stx-transfer? payout (as-contract tx-sender) winner))
                            ;; Update round state
                            (map-set rounds rid
                                (merge r {
                                    drawn: true,
                                    winner: (some winner),
                                })
                            )
                            (emit-drawn rid winner payout fee)
                            (ok true)
                        )
                        ;; This should never happen if tickets are properly indexed
                        (begin
                            (var-set rollover (+ (var-get rollover) (get pot r)))
                            (map-set rounds rid (merge r { drawn: true }))
                            (ok false)
                        )
                    )
                )
            )
        )
    )
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 
;; Read-Only Functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 
(define-read-only (get-round-info (rid uint))
    (map-get? rounds rid)
)

(define-read-only (get-commit-info
        (rid uint)
        (who principal)
    )
    (map-get? commits {
        round: rid,
        player: who,
    })
)

(define-read-only (get-ticket-owner
        (rid uint)
        (idx uint)
    )
    (map-get? tickets {
        round: rid,
        idx: idx,
    })
)

(define-read-only (get-current-round)
    (var-get round-id)
)

(define-read-only (get-admin)
    (var-get admin)
)

(define-read-only (get-treasury)
    (var-get treasury)
)

(define-read-only (get-rollover)
    (var-get rollover)
)

;; Check if a round is in sale phase
(define-read-only (is-sale-phase (rid uint))
    (within-sale rid)
)

;; Check if a round is in reveal phase
(define-read-only (is-reveal-phase (rid uint))
    (within-reveal rid)
)

;; Check if a round can be drawn
(define-read-only (can-draw (rid uint))
    (match (get-round rid)
        ok-r (and (>= stacks-block-height (get reveal-ends ok-r)) (not (get drawn ok-r)))
        err-r
        false
    )
)
