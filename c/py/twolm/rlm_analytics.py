#
# rlm_analytics.py
#
# This file is part of the reversi program
# http://github.com/rcrr/reversi
# 
# Aauthor Roberto Corradini mailto:rob_corradini@yahoo.it
# Copyright 2026 Roberto Corradini. All rights reserved.
#
# License
# 
# This program is free software; you can redistribute it and/or modify it
# under the terms of the GNU General Public License as published by the
# Free Software Foundation; either version 3, or (at your option) any
# later version.
# 
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
# GNU General Public License for more details.
# 
# You should have received a copy of the GNU General Public License
# along with this program; if not, write to the Free Software
# Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA
# or visit the site <http://www.gnu.org/licenses/>.
#

# twolm/rlm_analytics.py
from __future__ import annotations

import io
import sys
import numpy as np

from typing import TYPE_CHECKING, Dict, Optional, List
from datetime import datetime
from scipy.special import expit

from twolm.state_machine import Relevance

if TYPE_CHECKING:
    from twolm.logistic_model import RLMContext



__all__ = ['compute_training_metrics', 'format_console_report', 'format_file_report']



def _format_mobility_feature_table(ctx: "RLMContext", fid: int, indexes_col_offset: int, print_rows: bool = True) -> str:
    """
    Generates a detailed ASCII table for a specific mobility feature.
    """
    feature = ctx.feature_set.features[fid]
    n_configurations = feature.n_configurations
    n_instances = feature.n_instances
    offset = ctx.iwmap_feature_offset[fid]
    
    fallback_idx = int(ctx.feature_w_ranges[fid,0])
    fallback_freqs = {}
    if ctx.wmap_fallback is not None and len(ctx.wmap_fallback) > 0:
        for row in ctx.wmap_fallback:
            if int(row[0]) == fid:
                fallback_freqs[int(row[1])] = int(row[2])
                
    fallback_total_freq = sum(fallback_freqs.values())
    if fallback_idx != -1:
        fallback_weight = float(ctx.w[fallback_idx])
        fallback_str = f"(Fallback: w_idx={fallback_idx}, weight={fallback_weight:+.3f}, freq={fallback_total_freq:,})"
    else:
        fallback_str = "(Fallback: None)"

    f_wmap_mask = ctx.wmap[:, 0] == fid
    f_wmap = ctx.wmap[f_wmap_mask]
    true_configs_mask = f_wmap[:, 1] >= 0
    true_freqs = f_wmap[true_configs_mask, 2]
    true_config_ids = f_wmap[true_configs_mask, 1].astype(np.int64)
    w_indices = ctx.iwmap[offset + true_config_ids]
    true_weights = ctx.w[w_indices]
    if len(true_weights) > 0 and np.sum(true_freqs) > 0:
        weighted_mean = float(np.dot(true_weights, true_freqs) / np.sum(true_freqs))
    else:
        weighted_mean = 0.0

    cols = list(range(indexes_col_offset, indexes_col_offset + n_instances))
    train_indexes = ctx.rlm_indexes.indexes[:, cols]
    train_flat = train_indexes.ravel()
    
    y_true = ctx.game_values.astype(np.float64)
    y_repeated = np.repeat(y_true, n_instances)
    
    counts = np.bincount(train_flat, minlength=n_configurations)
    sum_y = np.bincount(train_flat, weights=y_repeated, minlength=n_configurations)
    sum_y2 = np.bincount(train_flat, weights=y_repeated**2, minlength=n_configurations)
    
    total_y_mean = np.mean(y_repeated)
    ss_total = np.sum((y_repeated - total_y_mean)**2)
    valid_mask = counts > 0
    group_means = np.zeros(n_configurations, dtype=np.float64)
    group_means[valid_mask] = sum_y[valid_mask] / counts[valid_mask]
    ss_between = np.sum(counts[valid_mask] * (group_means[valid_mask] - total_y_mean)**2)
    eta_squared = ss_between / ss_total if ss_total > 0 else 0.0

    count_c, count_ia, count_ic = 0, 0, 0
    for conf_id in range(n_configurations):
        w_dense_idx = offset + conf_id
        w_idx = ctx.iwmap[w_dense_idx]
        if conf_id in fallback_freqs:
            count_ic += 1
        elif w_idx >= 0:
            count_c += 1
        else:
            count_ia += 1

    header = (
        f"\nMOBILITY FEATURE: {feature.name} (fid={fid}) {fallback_str}\n"
        f"  Weighted Mean (w)           : {weighted_mean:+.4f}\n"
        f"  Instances / Cols            : {n_instances} {cols}\n"
        f"  Config Counts (C/IA/IC)     : {count_c} / {count_ia} / {count_ic}\n"
        f"  Eta-Squared (Discrimination): {eta_squared:.4f}\n"
        f"{'-' * 135}\n"
        f"  {'W_DENSE_IDX':<12} | {'STATUS':<8} | {'W_IDX':<8} | {'CONF_IDX':<8} | {'FREQUENCY':>10} | {'AVERAGE_GV':>10} | {'RMSE_GV':>8} | {'WEIGHT':>10}\n"
        f"{'-' * 135}\n"
    )
    
    if not print_rows:
        return header + f"  (Table rows omitted: feature not in config 'analytics.detailed_report')\n{'-' * 135}\n"
        
    rows = []
    for conf_id in range(n_configurations):
        w_dense_idx = offset + conf_id
        w_idx = ctx.iwmap[w_dense_idx]
        conf_idx_str = str(conf_id)
        freq = 0
        avg_gv = 0.0
        rmse_gv = 0.0
        
        if conf_id in fallback_freqs:
            status = 'IC'
            w_idx_str = 'NA'
            freq = fallback_freqs[conf_id]
            weight = float(ctx.w_dense[w_dense_idx])
        elif w_idx >= 0:
            status = 'C'
            w_idx_str = str(int(w_idx))
            freq = int(ctx.wmap[w_idx, 2])
            weight = float(ctx.w[w_idx])
        else:
            status = 'IA'
            w_idx_str = 'NA'
            weight = float(ctx.w_dense[w_dense_idx])
            
        # Calculate stats for any config that has occurrences in the dataset
        if counts[conf_id] > 0:
            avg_gv = sum_y[conf_id] / counts[conf_id]
            var = (sum_y2[conf_id] / counts[conf_id]) - (avg_gv**2)
            rmse_gv = np.sqrt(max(var, 0))
            
        weight_str = f"{weight:+.3f}"
        avg_gv_str = f"{avg_gv:>10.2f}" if freq > 0 else f"{'N/A':>10}"
        rmse_gv_str = f"{rmse_gv:>8.2f}" if freq > 0 else f"{'N/A':>8}"
        freq_str = f"{freq:>10,}"
                
        rows.append(
            f"  {w_dense_idx:<12} | {status:<8} | {w_idx_str:<8} | {conf_idx_str:<8} | {freq_str} | {avg_gv_str} | {rmse_gv_str} | {weight_str:>10}\n"
        )
        
    return header + "".join(rows) + f"{'-' * 135}\n"


def _format_pattern_feature_table(ctx: "RLMContext", fid: int, indexes_col_offset: int, print_rows: bool = True) -> str:
    """
    Generates a detailed ASCII table for a specific pattern feature.
    """
    feature = ctx.feature_set.features[fid]
    n_configurations = feature.n_configurations
    n_instances = feature.n_instances
    offset = ctx.iwmap_feature_offset[fid]
    
    fallback_idx = int(ctx.feature_w_ranges[fid,0])
    fallback_freqs = {}
    if ctx.wmap_fallback is not None and len(ctx.wmap_fallback) > 0:
        for row in ctx.wmap_fallback:
            if int(row[0]) == fid:
                fallback_freqs[int(row[1])] = int(row[2])
                
    fallback_total_freq = sum(fallback_freqs.values())
    if fallback_idx != -1:
        fallback_weight = float(ctx.w[fallback_idx])
        fallback_str = f"(Fallback: w_idx={fallback_idx}, weight={fallback_weight:+.3f}, freq={fallback_total_freq:,})"
    else:
        fallback_str = "(Fallback: None)"

    f_wmap_mask = ctx.wmap[:, 0] == fid
    f_wmap = ctx.wmap[f_wmap_mask]
    true_configs_mask = f_wmap[:, 1] >= 0
    true_freqs = f_wmap[true_configs_mask, 2]
    true_config_ids = f_wmap[true_configs_mask, 1].astype(np.int64)
    w_indices = ctx.iwmap[offset + true_config_ids]
    true_weights = ctx.w[w_indices]
    if len(true_weights) > 0 and np.sum(true_freqs) > 0:
        weighted_mean = float(np.dot(true_weights, true_freqs) / np.sum(true_freqs))
    else:
        weighted_mean = 0.0

    if len(true_weights) > 0:
        max_idx = np.argmax(true_weights)
        min_idx = np.argmin(true_weights)
        max_w = true_weights[max_idx]
        min_w = true_weights[min_idx]
        max_conf = true_config_ids[max_idx]
        min_conf = true_config_ids[min_idx]
        max_str = f"Max Weight (w)              : {max_w:+.4f} (conf: {max_conf})"
        min_str = f"Min Weight (w)              : {min_w:+.4f} (conf: {min_conf})"
    else:
        max_str = f"Max Weight (w)              : N/A"
        min_str = f"Min Weight (w)              : N/A"

    all_configs = np.arange(n_configurations, dtype=np.uint32)
    principal_configs = feature.ref.convert_to_principal_index(all_configs)

    cols = list(range(indexes_col_offset, indexes_col_offset + n_instances))
    train_indexes = ctx.rlm_indexes.indexes[:, cols]
    train_flat = train_indexes.ravel()
    
    y_true = ctx.game_values.astype(np.float64)
    y_repeated = np.repeat(y_true, n_instances)
    
    counts = np.bincount(train_flat, minlength=n_configurations)
    sum_y = np.bincount(train_flat, weights=y_repeated, minlength=n_configurations)
    sum_y2 = np.bincount(train_flat, weights=y_repeated**2, minlength=n_configurations)
    
    total_y_mean = np.mean(y_repeated)
    ss_total = np.sum((y_repeated - total_y_mean)**2)
    valid_mask = counts > 0
    group_means = np.zeros(n_configurations, dtype=np.float64)
    group_means[valid_mask] = sum_y[valid_mask] / counts[valid_mask]
    ss_between = np.sum(counts[valid_mask] * (group_means[valid_mask] - total_y_mean)**2)
    eta_squared = ss_between / ss_total if ss_total > 0 else 0.0

    count_c, count_ab, count_cl = 0, 0, 0
    for conf_id in range(n_configurations):
        w_dense_idx = offset + conf_id
        w_idx = ctx.iwmap[w_dense_idx]
        if conf_id in fallback_freqs:
            count_cl += 1
        elif w_idx >= 0:
            count_c += 1
        else:
            count_ab += 1

    header = (
        f"\nPATTERN FEATURE: {feature.name} (fid={fid}) {fallback_str}\n"
        f"  Weighted Mean (w)           : {weighted_mean:+.4f}\n"
        f"  {max_str}\n"
        f"  {min_str}\n"
        f"  Instances / Cols            : {n_instances} {cols}\n"
        f"  Config Counts (C/AB/CL)     : {count_c} / {count_ab} / {count_cl}\n"
        f"  Eta-Squared (Discrimination): {eta_squared:.4f}\n"
        f"{'-' * 135}\n"
        f"  {'W_DENSE_IDX':<12} | {'STATUS':<8} | {'W_IDX':<8} | {'CONF_IDX':<8} | {'PRINCIPAL_IDX':<14} | {'FREQUENCY':>10} | {'AVERAGE_GV':>10} | {'RMSE_GV':>8} | {'WEIGHT':>10}\n"
        f"{'-' * 135}\n"
    )
    
    if not print_rows:
        return header + f"  (Table rows omitted: feature not in config 'analytics.detailed_report')\n{'-' * 135}\n"
    
    rows = []
    for conf_id in range(n_configurations):
        w_dense_idx = offset + conf_id
        w_idx = ctx.iwmap[w_dense_idx]
        conf_idx_str = str(conf_id)
        principal_idx_str = str(principal_configs[conf_id])
        freq = 0
        avg_gv = 0.0
        rmse_gv = 0.0
        
        if conf_id in fallback_freqs:
            status = 'CL'
            w_idx_str = 'NA'
            freq = fallback_freqs[conf_id]
            weight = float(ctx.w_dense[w_dense_idx])
        elif w_idx >= 0:
            status = 'C'
            w_idx_str = str(int(w_idx))
            freq = int(ctx.wmap[w_idx, 2])
            weight = float(ctx.w[w_idx])
        else:
            status = 'AB'
            w_idx_str = 'NA'
            weight = float(ctx.w_dense[w_dense_idx])
            
        # Calculate stats for any config that has occurrences in the dataset
        if counts[conf_id] > 0:
            avg_gv = sum_y[conf_id] / counts[conf_id]
            var = (sum_y2[conf_id] / counts[conf_id]) - (avg_gv**2)
            rmse_gv = np.sqrt(max(var, 0))
            
        weight_str = f"{weight:+.3f}"
        avg_gv_str = f"{avg_gv:>10.2f}" if freq > 0 else f"{'N/A':>10}"
        rmse_gv_str = f"{rmse_gv:>8.2f}" if freq > 0 else f"{'N/A':>8}"
        freq_str = f"{freq:>10,}"
                
        rows.append(
            f"  {w_dense_idx:<12} | {status:<8} | {w_idx_str:<8} | {conf_idx_str:<8} | {principal_idx_str:<14} | {freq_str} | {avg_gv_str} | {rmse_gv_str} | {weight_str:>10}\n"
        )
        
    return header + "".join(rows) + f"{'-' * 135}\n"


def _format_header(ctx: "RLMContext", detailed: bool = False) -> str:
    """
    Formats the common header metrics table for both console and file reports.
    """
    model_name = ctx.cfg.name
    total_params = len(ctx.w)
    dense_params = len(ctx.w_dense) if ctx.w_dense is not None else 0
    
    opt_info = ctx.opt_info or {}
    opt_status = opt_info.get('reason', 'N/A')
    opt_iters = opt_info.get('iters', 0)
    
    # Read metrics from context
    train_metrics = ctx.train_metrics or {}
    val_metrics = ctx.vld_metrics
    
    train_rmse = train_metrics.get('train_rmse_y', 0.0)
    train_mae = train_metrics.get('train_mae_y', 0.0)
    train_loss = train_metrics.get('train_loss', 0.0)
    train_samples = train_metrics.get('train_samples', 0)
    
    if val_metrics:
        val_rmse_str = f"{val_metrics.get('vld_rmse_y', 0.0):.2f}"
        val_mae_str = f"{val_metrics.get('vld_mae_y', 0.0):.2f}"
        val_loss_str = f"{val_metrics.get('vld_loss', 0.0):.4e}"
        val_samples = val_metrics.get('vld_samples', 0)
        gen_gap = val_metrics.get('vld_rmse_y', 0.0) - train_rmse
    else:
        val_rmse_str = val_mae_str = val_loss_str = "N/A"
        val_samples = 0
        gen_gap = 0.0

    pop_rmse_str = f"{train_metrics.get('pop_rmse_y', 0.0):.2f}"
    pop_mae_str = f"{train_metrics.get('pop_mae_y', 0.0):.2f}"
    pop_loss_str = f"{train_metrics.get('pop_loss', 0.0):.4e}"

    header = f"\n{'=' * 135}\nMODEL ANALYTICS REPORT: {model_name}\n{'=' * 135}\n"
    
    if detailed:
        creation_date = datetime.now().strftime('%Y-%m-%d %H:%M:%S')
        stat_model_cfg = ctx.cfg.stat_model
        
        header += f"  Creation Date               : {creation_date}\n"
        header += f"  Training/Validation Records : {train_samples:,} / {val_samples:,}\n"
        header += f"  Frequency Cut-Off           : {stat_model_cfg.frequency_cut_off}\n"
        header += f"  Logit Clipping              : {stat_model_cfg.logit_clipping}\n"
        header += f"  Ridge Regularization        : {stat_model_cfg.ridge_regularization}\n"

    header += (
        f"  Total Parameters (W)        : {total_params:,}\n"
        f"  Dense Parameters (K)        : {dense_params:,}\n"
        f"  Optimization Status         : {opt_status} ({opt_iters} iters)\n"
        f"{'-' * 135}\n"
        f"  {'METRIC':<15} | {'TRAINING':<14} | {'VALIDATION':<14} | {'POPULATION':<14}\n"
        f"{'-' * 135}\n"
        f"  {'RMSE (y)':<15} | {train_rmse:<14.2f} | {val_rmse_str:<14} | {pop_rmse_str:<14}\n"
        f"  {'MAE (y)':<15} | {train_mae:<14.2f} | {val_mae_str:<14} | {pop_mae_str:<14}\n"
        f"  {'Loss (MSE/2)':<15} | {train_loss:<14.4e} | {val_loss_str:<14} | {pop_loss_str:<14}\n"
        f"{'-' * 135}\n"
        f"  Generalization Gap (RMSE)   : {gen_gap:.2f}\n"
        f"{'=' * 135}\n"
    )
    return header


def format_console_report(ctx: "RLMContext") -> str:
    """
    Formats the short summary report for console output.
    """
    return _format_header(ctx, detailed=False)


def format_file_report(ctx: "RLMContext") -> str:
    """
    Formats the complete detailed report for TXT file output.
    """
    report = _format_header(ctx, detailed=True)

    buffer = io.StringIO()
    ctx.feature_set.print_summary(output=buffer)
    feature_summary = buffer.getvalue()
    report += f"\nFEATURE SET SUMMARY:\n{'-' * 135}\n{feature_summary}\n"
    
    # Safely read the config list
    detailed_report_list = []
    if hasattr(ctx.cfg, 'analytics') and ctx.cfg.analytics is not None:
        detailed_report_list = getattr(ctx.cfg.analytics, 'detailed_report', [])
    
    report += f"MOBILITY FEATURES DETAILS:\n{'-' * 135}\n"
    col_offset = 0
    for fid, feature in enumerate(ctx.feature_set.features):
        if feature.category == 1:  # Mobility
            print_rows = feature.name in detailed_report_list
            report += _format_mobility_feature_table(ctx, fid, col_offset, print_rows=print_rows)
        col_offset += feature.n_instances
        
    report += f"\nPATTERN FEATURES DETAILS:\n{'-' * 135}\n"
    col_offset = 0
    for fid, feature in enumerate(ctx.feature_set.features):
        if feature.category == 2:  # Pattern
            print_rows = feature.name in detailed_report_list
            report += _format_pattern_feature_table(ctx, fid, col_offset, print_rows=print_rows)
        col_offset += feature.n_instances

    return report


def compute_training_metrics(ctx: "RLMContext") -> Dict[str, float]:
    """
    Computes MAE(y) and RMSE(y) on the training set using the optimized weights (w).
    Also computes baseline (population mean) metrics for comparison.
    """
    ctx.log_event(Relevance.INFO, "Computing metrics on the training set...")
    
    # --- Model Metrics ---
    linear_predictor = np.sum(ctx.w[ctx.design_matrix].astype(np.float64), axis=1)
    z_pred = expit(linear_predictor)
    
    y_pred = ctx.z2y(z_pred)
    y_true = ctx.game_values.astype(np.float64)
    
    rn_y = y_pred - y_true
    mae_y = np.mean(np.abs(rn_y))
    rmse_y = np.sqrt(np.dot(rn_y, rn_y) / len(rn_y))
    
    # --- Population (Baseline) Metrics ---
    y_mean = np.mean(y_true)
    rn_y_pop = y_true - y_mean
    pop_mae_y = np.mean(np.abs(rn_y_pop))
    pop_rmse_y = np.sqrt(np.dot(rn_y_pop, rn_y_pop) / len(rn_y_pop))
    
    z_true = ctx.y2z(ctx.game_values)
    z_mean = np.mean(z_true)
    rn_z_pop = z_true - z_mean
    pop_loss = 0.5 * (np.dot(rn_z_pop, rn_z_pop) / len(rn_z_pop))
    
    return {
        'train_mae_y': float(mae_y),
        'train_rmse_y': float(rmse_y),
        'train_loss': float(ctx.opt_info['f']) if ctx.opt_info else 0.0,
        'train_samples': int(len(y_true)),
        'pop_mae_y': float(pop_mae_y),
        'pop_rmse_y': float(pop_rmse_y),
        'pop_loss': float(pop_loss)
    }


