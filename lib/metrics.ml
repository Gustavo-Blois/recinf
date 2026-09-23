(* Evaluation metrics. A ranking is a list of doc ids, best first. Binary
   metrics treat grade >= 1 as relevant; -1 and unjudged docs are not relevant
   (they never enter [Query.judgments]). *)

let cutoff = 10

let rec take k = function
  | x :: rest when k > 0 -> x :: take (k - 1) rest
  | _ -> []

let hits_at k ~relevant ranking =
  List.length (List.filter (fun d -> List.mem d relevant) (take k ranking))

let precision_at k ~relevant ranking =
  float_of_int (hits_at k ~relevant ranking) /. float_of_int k

let recall_at k ~relevant ranking =
  match relevant with
  | [] -> 0.0
  | _ -> float_of_int (hits_at k ~relevant ranking) /. float_of_int (List.length relevant)

let f1_at k ~relevant ranking =
  let p = precision_at k ~relevant ranking and r = recall_at k ~relevant ranking in
  if p +. r = 0.0 then 0.0 else 2.0 *. p *. r /. (p +. r)

(* AP = (1/|R|) * sum over relevant retrieved docs of precision at their rank;
   relevant docs never retrieved contribute 0. *)
let average_precision ~relevant ranking =
  match relevant with
  | [] -> 0.0
  | _ ->
      let _, _, sum =
        List.fold_left
          (fun (i, hits, sum) d ->
            if List.mem d relevant then
              (i + 1, hits + 1, sum +. (float_of_int (hits + 1) /. float_of_int (i + 1)))
            else (i + 1, hits, sum))
          (0, 0, 0.0) ranking
      in
      sum /. float_of_int (List.length relevant)

(* AP@k: only the first k documents count, but the sum is still divided by the
   total number of relevant documents (like trec_eval's map_cut). *)
let average_precision_at k ~relevant ranking = average_precision ~relevant (take k ranking)

let reciprocal_rank ~relevant ranking =
  let rec go i = function
    | [] -> 0.0
    | d :: rest -> if List.mem d relevant then 1.0 /. float_of_int i else go (i + 1) rest
  in
  go 1 ranking

(* Graded gain: grade 1 (complete answer) -> 4 ... grade 4 (minimum interest) -> 1.
   DCG@k = sum gain_i / log2(i + 1). *)
let gain grade = float_of_int (5 - grade)

let dcg gains =
  let _, s =
    List.fold_left
      (fun (i, s) g -> (i + 1, s +. (g /. Float.log2 (float_of_int (i + 1)))))
      (1, 0.0) gains
  in
  s

let ndcg_at k ~judgments ranking =
  let gains =
    List.map
      (fun d -> match List.assoc_opt d judgments with Some g -> gain g | None -> 0.0)
      (take k ranking)
  in
  let ideal =
    judgments
    |> List.map (fun (_, g) -> gain g)
    |> List.sort (fun a b -> Float.compare b a)
    |> take k
  in
  let idcg = dcg ideal in
  if idcg = 0.0 then 0.0 else dcg gains /. idcg

type query_metrics = {
  query : int;
  p10 : float;
  r10 : float;
  f1_10 : float;
  ap : float;  (* over the whole ranking (AP@all) *)
  ap10 : float;
  rr : float;
  ndcg10 : float;
}

let evaluate_query (q : Query.query) ranking =
  let relevant = q.relevant_documents in
  { query = q.index;
    p10 = precision_at cutoff ~relevant ranking;
    r10 = recall_at cutoff ~relevant ranking;
    f1_10 = f1_at cutoff ~relevant ranking;
    ap = average_precision ~relevant ranking;
    ap10 = average_precision_at cutoff ~relevant ranking;
    rr = reciprocal_rank ~relevant ranking;
    ndcg10 = ndcg_at cutoff ~judgments:q.judgments ranking }

type summary = {
  mean_p10 : float;
  mean_r10 : float;
  mean_f1_10 : float;
  map : float;
  map10 : float;
  mrr : float;
  mean_ndcg10 : float;
}

(* Macro average over queries; MAP is the mean of [ap]. *)
let aggregate (ms : query_metrics list) =
  let mean f = List.fold_left (fun a m -> a +. f m) 0.0 ms /. float_of_int (List.length ms) in
  { mean_p10 = mean (fun m -> m.p10);
    mean_r10 = mean (fun m -> m.r10);
    mean_f1_10 = mean (fun m -> m.f1_10);
    map = mean (fun m -> m.ap);
    map10 = mean (fun m -> m.ap10);
    mrr = mean (fun m -> m.rr);
    mean_ndcg10 = mean (fun m -> m.ndcg10) }

(* Selecting one per-query metric by name, for comparisons. *)
type metric = Ap | Ap10 | P10 | R10 | F1 | Rr | Ndcg10

let all_metrics = [ Ap; Ap10; P10; R10; F1; Rr; Ndcg10 ]

let value m (q : query_metrics) =
  match m with
  | Ap -> q.ap
  | Ap10 -> q.ap10
  | P10 -> q.p10
  | R10 -> q.r10
  | F1 -> q.f1_10
  | Rr -> q.rr
  | Ndcg10 -> q.ndcg10

(* per-query name *)
let name = function
  | Ap -> "AP@all" | Ap10 -> "AP@10" | P10 -> "P@10" | R10 -> "R@10" | F1 -> "F1@10" | Rr -> "RR" | Ndcg10 -> "NDCG@10"

(* name of its mean over queries *)
let mean_name = function Ap -> "MAP" | Ap10 -> "MAP@10" | Rr -> "MRR" | m -> name m

let metric_of_string s =
  match String.lowercase_ascii s with
  | "ap" | "map" | "ap@all" | "apall" -> Some Ap
  | "ap10" | "ap@10" | "map10" | "map@10" -> Some Ap10
  | "p10" | "p@10" -> Some P10
  | "r10" | "r@10" -> Some R10
  | "f1" | "f1@10" -> Some F1
  | "rr" | "mrr" -> Some Rr
  | "ndcg" | "ndcg10" | "ndcg@10" -> Some Ndcg10
  | _ -> None
