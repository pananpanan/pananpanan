//配对Wilcoxon秩和检验
import org.apache.commons.math3.stat.inference.WilcoxonSignedRankTest;

public class PairedWilcoxonTestExample {
    public static void main(String[] args) {
        // 示例数据
        double[] before = {2, 5, 2}; // 第一组数据，在某个实验前的观测值
        double[] after = {3, 6, 3}; // 第二组数据，在相同实验后的观测值

        // 执行配对Wilcoxon秩和检验
        WilcoxonSignedRankTest wilcoxonTest = new WilcoxonSignedRankTest();
        double pValue = wilcoxonTest.wilcoxonSignedRankTest(before, after, false); // 进行配对Wilcoxon秩和检验

        // 输出结果
        System.out.println("p-value: " + pValue);
    }
}


//非配对Wilcoxon秩和检验
import org.apache.commons.math3.stat.inference.MannWhitneyUTest;

public class IndependentWilcoxonTestExample {
    public static void main(String[] args) {
        // 示例数据
        double[] group1 = {2, 5, 2}; // 第一组数据
        double[] group2 = {3, 6, 3}; // 第二组数据

        // 执行独立Wilcoxon秩和检验
        MannWhitneyUTest wilcoxonTest = new MannWhitneyUTest();
        double pValue = wilcoxonTest.mannWhitneyUTest(group1, group2); // 进行独立Wilcoxon秩和检验

        // 输出结果
        System.out.println("p-value: " + pValue);
    }
}


//p值调整
import org.apache.commons.math3.stat.inference.MannWhitneyUTest;
import org.apache.commons.math3.stat.inference.MultipleTestingCorrection;

public class PValueAdjustmentExample {
    public static void main(String[] args) {
        // 示例数据
        double[] group1 = {0.05, 0.02, 0.01, 0.03}; // 第一组数据
        double[] group2 = {0.1, 0.07, 0.12, 0.08}; // 第二组数据

        // 执行独立Wilcoxon秩和检验
        MannWhitneyUTest wilcoxonTest = new MannWhitneyUTest();
        double pValue = wilcoxonTest.mannWhitneyUTest(group1, group2); // 进行独立Wilcoxon秩和检验

        // 进行p值调整（使用BH算法）
        double[] pValues = {pValue}; // 将原始p值放入数组中
        double[] adjustedPValues = MultipleTestingCorrection.bhCorrection(pValues); // BH算法进行p值调整

        // 获取调整后的p值
        double adjustedPValue = adjustedPValues[0];

        // 输出结果
        System.out.println("Unadjusted p-value: " + pValue);
        System.out.println("Adjusted p-value (BH): " + adjustedPValue);
    }
}




