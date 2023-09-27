//配对t检验
import org.apache.commons.math3.stat.StatUtils;
import org.apache.commons.math3.distribution.TDistribution;
import org.apache.commons.math3.distribution.TDistributionImpl;

public class PairedTTestExample {
    public static void main(String[] args) {
        // 示例数据
        double[] before = {2, 5, 2}; // 第一组数据，在某个实验前的观测值
        double[] after = {3, 6, 3}; // 第二组数据，在相同实验后的观测值

        // 计算配对差异
        double[] differences = new double[before.length];
        for (int i = 0; i < before.length; i++) {
            differences[i] = after[i] - before[i];
        }

        // 计算配对差异的平均值和标准差
        double meanDifference = StatUtils.mean(differences);
        double sdDifference = Math.sqrt(StatUtils.variance(differences));

        // 计算样本数量
        int n = before.length;

        // 计算t值
        double tValue = meanDifference / (sdDifference / Math.sqrt(n));

        // 计算自由度
        int df = n - 1;

        // 使用TDistribution类计算p-value
        TDistribution tDistribution = new TDistributionImpl(df);
        double pValue = 2 * tDistribution.cumulativeProbability(-Math.abs(tValue));

        // 输出结果
        System.out.println("t-value: " + tValue);
        System.out.println("p-value: " + pValue);
    }
}


//非配对t检验
import org.apache.commons.math3.stat.inference.TTest;

public class IndependentTTestExample {
    public static void main(String[] args) {
        // 示例数据
        double[] group1 = {2, 5, 2}; // 第一组数据
        double[] group2 = {3, 6, 3}; // 第二组数据

        // 执行非配对t检验
        TTest tTest = new TTest();
        double pValue = tTest.homoscedasticTTest(group1, group2); // 进行非配对t检验

        // 输出结果
        System.out.println("p-value: " + pValue);
    }
}
